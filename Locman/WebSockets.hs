{-# LANGUAGE QuasiQuotes, TemplateHaskell, TypeFamilies, OverloadedStrings, BangPatterns #-}

module Locman.WebSockets
( joinSession
, runSockets
)where

import Model
import Data.Text
import Control.Exception.Base
import qualified Network.WebSockets 
import qualified Data.Map as M
import LocMan.Types
import LocMan.Conduit
import LocMan.Conduit.WebSockets
import Control.Lens
import Control.Concurrent.STM.TVar
import Control.Concurrent.STM
import Control.Monad.IO.Class
import Control.Monad.Trans.Reader(runReaderT)
import Data.Conduit
import Data.Conduit.TMChan
import qualified Data.List as L (delete)
import Network.WebSockets(Connection)

runSockets :: Connection -> User -> TVar UserLocationSession -> IO ()
runSockets connection currentUsr sess = do
  session <- atomically $ readTVar sess
  let jUser = userToJUser currentUsr
  let masterChannel = session^.sessionMasterChannel
  dupedChan <- atomically $ do
    writeTMChan masterChannel $ flip UserSessionEvent Joined jUser
    dupTMChan masterChannel
  finally (runReaderT (race_ 
            (runInlet jUser masterChannel)
            (runOutlet jUser dupedChan)
            )
            connection
            )
            $ atomically $ leaveSession currentUsr sess

runInlet :: MonadIO m => JUser -> TMChan UserSessionEvent -> WebSocketsT m ()
runInlet jUser masterChannel = sourceWS $$ toByteStringConduit =$= decodeConduit =$= errorReportConduit =$= addAuthorConduit jUser =$ sinkTMChan masterChannel False 

runOutlet :: MonadIO m => JUser -> TMChan UserSessionEvent -> WebSocketsT m ()
runOutlet jUser dupedChan = sourceTMChan dupedChan $= dropWithUserConduit jUser =$= encodeConduit $$ sinkWSText

-- | get or create session if not exists
retrieveSession :: Text -> AppStatus -> STM (TVar UserLocationSession)
retrieveSession sid shared = do
  case M.lookup sid shared of
    Just sessionTVar -> return sessionTVar
    Nothing -> do
      nChan <- newBroadcastTMChan
      newTVar $ UserLocationSession [] nChan 

addCurrentUserSession :: User -> UserLocationSession -> UserLocationSession
addCurrentUserSession currentUser session = session & sessionUsers .~ (currentUser : _sessionUsers session)
 
deleteUserFromSession :: User -> UserLocationSession -> UserLocationSession
deleteUserFromSession currentUser session = session & sessionUsers .~ (L.delete currentUser $ _sessionUsers session)

joinSession :: User -> TVar AppStatus -> LocationSessionId -> STM (TVar UserLocationSession)
joinSession user apps sid = do
  sharedStates <- readTVar apps
  userSessionTVar <- retrieveSession sid $ sharedStates

  writeTVar userSessionTVar =<< return . addCurrentUserSession user =<< readTVar userSessionTVar
  writeTVar apps $ M.insert sid userSessionTVar sharedStates 
  return userSessionTVar


leaveSession :: User -> TVar UserLocationSession -> STM ()
leaveSession currentUsr session = do
  currSess <- readTVar session
  writeTMChan (currSess^.sessionMasterChannel) (flip UserSessionEvent Exited $ userToJUser currentUsr)
  let newSess = deleteUserFromSession currentUsr currSess
  writeTVar session newSess

