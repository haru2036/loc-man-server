
{-# LANGUAGE QuasiQuotes, TemplateHaskell, TypeFamilies, OverloadedStrings #-}

module Handler.WebSocket where

import Import
import Yesod.Core
import Yesod.WebSockets
import qualified Data.Text.Lazy as TL
import Control.Monad (forever)
import Control.Monad.Trans.Reader
import Control.Concurrent (threadDelay)
import Data.Time
import Conduit
import LocMan.Types


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

-- | get session or create session if not exists
checkSession :: LocationSessionId -> Handler UserLocationSession
checkSession sid = do
  app <- getYesod
  sessions <- atomically $ readTChan $ appTChan app
  case lookup sid sessions of
    Just session -> return session
    Nothing -> do
      nChan <- atomically newBroadcastTChan
      return $ UserLocationSession [] nChan 
