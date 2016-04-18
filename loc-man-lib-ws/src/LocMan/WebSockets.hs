{-# LANGUAGE QuasiQuotes, TemplateHaskell, TypeFamilies, OverloadedStrings, BangPatterns, RankNTypes #-}

module LocMan.WebSockets
( joinSession
, runSockets
)where

import Data.Text
import Control.Exception.Base
import qualified Data.Map as M
import LocMan.Types
import LocMan.Conduit.Common
import LocMan.Conduit.WebSockets
import Control.Concurrent.STM.TVar
import Control.Concurrent.STM
import Control.Monad.IO.Class
import Control.Monad.Trans.Reader(runReaderT)
import Data.Conduit
import Data.Conduit.TMChan
import qualified Data.List as L (delete)
import Network.WebSockets(Connection)

runSockets :: ModelUser a => a -> TVar (UserLocationSession a) -> Connection -> IO ()
runSockets currentUsr sess connection = do
  session <- atomically $ readTVar sess
  let jUser = toJUser currentUsr
  let masterChannel = sessionMasterChannel session
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
retrieveSession :: ModelUser a => Text -> AppStatus a -> STM (TVar (UserLocationSession a))
retrieveSession sid shared = do
  case M.lookup sid shared of
    Just sessionTVar -> return sessionTVar
    Nothing -> do
      nChan <- newBroadcastTMChan
      newTVar $ UserLocationSession [] nChan 

addCurrentUserSession :: forall a. ModelUser a => a -> UserLocationSession a -> UserLocationSession a
addCurrentUserSession currentUser session = session{sessionUsers = (currentUser : (sessionUsers session))}
 
deleteUserFromSession :: forall a. ModelUser a => a -> UserLocationSession a -> UserLocationSession a
deleteUserFromSession currentUser session = session{sessionUsers = (L.delete currentUser $ sessionUsers session)}

joinSession :: ModelUser a => a -> TVar (AppStatus a)-> LocationSessionId -> STM (TVar (UserLocationSession a))
joinSession user apps sid = do
  sharedStates <- readTVar apps
  userSessionTVar <- retrieveSession sid $ sharedStates

  writeTVar userSessionTVar =<< return . addCurrentUserSession user =<< readTVar userSessionTVar
  writeTVar apps $ M.insert sid userSessionTVar sharedStates 
  return userSessionTVar


leaveSession :: ModelUser a => a -> TVar (UserLocationSession a) -> STM ()
leaveSession currentUsr session = do
  currSess <- readTVar session
  writeTMChan (sessionMasterChannel currSess) (flip UserSessionEvent Exited $ toJUser currentUsr)
  let newSess = deleteUserFromSession currentUsr currSess
  writeTVar session newSess

