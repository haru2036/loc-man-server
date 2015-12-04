{-# LANGUAGE QuasiQuotes, TemplateHaskell, TypeFamilies, OverloadedStrings, BangPatterns #-}

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
import Data.Maybe(fromJust)
import Data.Void(Void)
import Debug.Trace
import Control.DeepSeq


timeSource :: MonadIO m => Source m TL.Text
timeSource = forever $ do
    now <- liftIO getCurrentTime
    yield $ TL.pack $ show now
    liftIO $ threadDelay 5000000

getWebSocketSessionR ::Text -> Handler ()
getWebSocketSessionR = receiveWebSockets

getWebSocketR :: Handler Html
getWebSocketR = do
    defaultLayout $
        toWidget
            [julius|
                var conn = new WebSocket("ws://mbp:3000/session/ws/1");
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

receiveWebSockets :: LocationSessionId -> Handler ()
receiveWebSockets id = do
  maybeuser <- maybeAuth
  app <- getYesod
  sess <- atomically $ joinSession (entityVal $ fromJust maybeuser) app id
  webSockets $ runSocket sess

runSocket :: TVar UserLocationSession -> WebSocketsT Handler ()
runSocket x = do
  session <- atomically $ readTVar x
  race_
        (sourceWS $$ mapC TL.toUpper =$= traceConduit =$ (locationSink $ session^.sessionMasterChannel))
        ((locationSource $ session^.sessionMasterChannel) $$ sinkWSText)
 
locationSource :: MonadIO m => TChan UserLocationRecord -> Source m UserLocationRecord
locationSource chan = do
  forever $ do
    record <- atomically $ readTChan =<< dupTChan chan 
    yield record

locationSink :: MonadIO m => TChan UserLocationRecord -> Sink UserLocationRecord m ()
locationSink chan = do 
  awaitForever $ do
    atomically . writeTChan chan

traceConduit :: (MonadIO m, Show a) => Conduit a m a
traceConduit = do
  awaitForever $ \x -> do
    !a <- return $ trace ("passed value is :" ++ show x) x
    yield a


-- | get or create session if not exists
retrieveSession :: Text -> AppStates -> STM (TVar UserLocationSession)
retrieveSession sid shared = do
  case M.lookup sid shared of
    Just sessionTVar -> return sessionTVar
    Nothing -> do
      nChan <- newBroadcastTChan
      newTVar $ UserLocationSession [] nChan 

addCurrentUserSession :: User -> UserLocationSession -> UserLocationSession
addCurrentUserSession currentUser session = session & sessionUsers .~ (currentUser : _sessionUsers session)
 
joinSession :: User -> App -> LocationSessionId -> STM (TVar UserLocationSession)
joinSession user app sid = do
     sharedStates <- readTVar $ appSharedStates app
     userSessionTVar <- retrieveSession sid $ sharedStates
     userSession <- readTVar userSessionTVar
     let newSession = addCurrentUserSession user userSession 
     writeTVar userSessionTVar newSession
     let newState = M.insert sid userSessionTVar sharedStates 
     writeTVar (appSharedStates app) newState
     return userSessionTVar


