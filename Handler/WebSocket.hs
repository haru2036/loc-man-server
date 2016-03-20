{-# LANGUAGE QuasiQuotes, TemplateHaskell, TypeFamilies, OverloadedStrings, BangPatterns #-}

module Handler.WebSocket
( getWebSocketSessionR
, getWebSocketR
, joinSession
)where

import Import
import Yesod.Core()
import Yesod.WebSockets
import qualified Data.Text.Lazy as TL
import Control.Concurrent (threadDelay)
import qualified Data.Map as M
import LocMan.Types
import LocMan.Conduit
import Control.Lens
import Data.Maybe(fromJust)
import Control.Concurrent.STM.TVar()
import Data.Conduit.TMChan
import qualified Data.List as L (delete)


timeSource :: MonadIO m => Source m TL.Text
timeSource = forever $ do
    now <- liftIO getCurrentTime
    yield $ TL.pack $ show now
    liftIO $ threadDelay 5000000

getWebSocketSessionR :: Text -> Handler ()
getWebSocketSessionR = receiveWebSockets

getWebSocketR :: Handler Html
getWebSocketR = do
    defaultLayout $
        toWidget
            [julius|
                var conn = new WebSocket("ws://localhost:3000/api/session/ws/1");
                conn.onopen = function() {
                    document.write("<p>open!</p>");
                    document.write("<button id=button>Send another message</button>")
                    document.getElementById("button").addEventListener("click", function(){
                        var msg = prompt("Enter a message for the server");
                        conn.send(msg);
                    });
                    conn.send('{"tag":"UpdateLocation","contents":{"accuracy":100,"latitude":100,"altitude":135,"longitude":100}}');
                };
                conn.onmessage = function(e) {
                    document.write("<p>" + e.data + "</p>");
                };
            |]

receiveWebSockets :: LocationSessionId -> Handler ()
receiveWebSockets ident = do
  maybeuser <- maybeAuth
  app <- getYesod
  let usr = entityVal $ fromJust maybeuser
  sess <- atomically $ joinSession usr app ident
  webSockets $ runSocket usr sess

runSocket :: User -> TVar UserLocationSession -> WebSocketsT Handler ()
runSocket currentUsr sess = do
  session <- atomically $ readTVar sess
  let jUser = userToJUser currentUsr
  let masterChannel = session^.sessionMasterChannel
  dupedChan <- atomically $ do
    writeTMChan masterChannel $ flip UserSessionEvent Joined $ jUser
    dupTMChan masterChannel
  finally (race_
        (sourceWS $$ toByteStringConduit =$= decodeConduit =$= errorReportConduit =$= addAuthorConduit jUser =$ sinkTMChan masterChannel False)
        (sourceTMChan dupedChan $= dropWithUserConduit jUser =$= encodeConduit $$ sinkWSText)) $ atomically $ leaveSession currentUsr sess


-- | get or create session if not exists
retrieveSession :: Text -> AppStates -> STM (TVar UserLocationSession)
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

joinSession :: User -> App -> LocationSessionId -> STM (TVar UserLocationSession)
joinSession user app sid = do
     sharedStates <- readTVar $ appSharedStates app
     userSessionTVar <- retrieveSession sid $ sharedStates

     writeTVar userSessionTVar =<< return . addCurrentUserSession user =<< readTVar userSessionTVar
     writeTVar (appSharedStates app) $ M.insert sid userSessionTVar sharedStates 
     return userSessionTVar


leaveSession :: User -> TVar UserLocationSession -> STM ()
leaveSession currentUsr session = do
  currSess <- readTVar session
  writeTMChan (currSess^.sessionMasterChannel) (flip UserSessionEvent Exited $ userToJUser currentUsr)
  let newSess = deleteUserFromSession currentUsr currSess
  writeTVar session newSess

