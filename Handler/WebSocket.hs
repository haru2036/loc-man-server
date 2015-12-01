{-# LANGUAGE QuasiQuotes, TemplateHaskell, TypeFamilies, OverloadedStrings #-}

module Handler.WebSocket
( getWebSocketSessionR
, getWebSocketR
)where

import Import
import Yesod.Core
import Yesod.WebSockets
import qualified Data.Text.Lazy as TL
import Control.Monad (forever)
import Control.Monad.Trans.Reader
import Control.Concurrent (threadDelay)
import Data.Time
import qualified Data.Map as M
import Data.Map.Strict
import Conduit
import LocMan.Types
import Control.Concurrent.STM.TVar
import Control.Lens


timeSource :: MonadIO m => Source m TL.Text
timeSource = forever $ do
    now <- liftIO getCurrentTime
    yield $ TL.pack $ show now
    liftIO $ threadDelay 5000000

getWebSocketSessionR :: Handler ()
getWebSocketSessionR = receiveWebSockets

getWebSocketR :: Handler Html
getWebSocketR = do
    defaultLayout $
        toWidget
            [julius|
                var conn = new WebSocket("ws://mbp:3000/session/ws");
                conn.onopen = function() {
                    document.write("<p>open!</p>");
                    document.write("<button id=button>Send another message</button>")
                    document.getElementById("button").addEventListener("click", function(){
                        var msg = prompt("Enter a message for the server");
                        conn.send(msg);
                    });
                    conn.send("hello world");
                };
                conn.onmessage = function(e) {
                    document.write("<p>" + e.data + "</p>");
                };
            |]

receiveWebSockets :: Handler ()
receiveWebSockets = webSockets socketApp 

socketApp :: WebSocketsT Handler ()
socketApp = do
  race_
        (sourceWS $$ mapC TL.toUpper =$ sinkWSText)
        (timeSource $$ sinkWSText)

-- | get or create session if not exists
retrieveSession :: Text -> SharedStates -> STM UserLocationSession
retrieveSession sid shared = do
  case M.lookup sid shared of
    Just session -> do
      return session
    Nothing -> do
      nChan <- newBroadcastTChan
      return $ UserLocationSession [] nChan 

addCurrentUserToSharedStates :: Text -> User -> UserLocationSession -> TVar SharedStates -> STM () 
addCurrentUserToSharedStates sid currentUser session sessionListTVar = do
  let users = _sessionUsers session
  let newUsers = currentUser : users 
  let newSession = session & sessionUsers .~ newUsers 
  sharedStates <- readTVar sessionListTVar
  let newState = M.insert sid newSession sharedStates
  writeTVar sessionListTVar newState

joinSession :: User -> Text -> App -> Handler UserLocationSession
joinSession user sid app = atomically $ do
     sharedStates <- readTVar $ appSharedStates app
     userSession <- retrieveSession sid sharedStates
     addCurrentUserToSharedStates sid user userSession $ appSharedStates app
     return userSession
