{-# LANGUAGE Rank2Types #-}

module LocMan.Conduit.WebSockets where

import Control.Monad
import Control.Monad.IO.Class
import Conduit
import qualified Network.WebSockets as WS
import qualified Data.Conduit.List as CL
import Control.Monad.Trans.Reader

-- this file contains some codes from yesod-websockets
--
type WebSocketsT = ReaderT WS.Connection

receiveData :: (MonadIO m, WS.WebSocketsData a) => WebSocketsT m a
receiveData = ReaderT $ liftIO . WS.receiveData

wrapWS :: (MonadIO m, WS.WebSocketsData a) => (WS.Connection -> a -> IO ()) -> a -> WebSocketsT m ()
wrapWS ws x = ReaderT $ liftIO . flip ws x


-- | A @Source@ of WebSockets data from the user.
--
-- Since 0.1.0
sourceWS :: (MonadIO m, WS.WebSocketsData a) => Producer (WebSocketsT m) a
sourceWS = forever $ lift receiveData >>= yield


-- | A @Sink@ for sending textual data to the user.
--
-- Since 0.1.0
sinkWSText :: (MonadIO m, WS.WebSocketsData a) => Consumer a (WebSocketsT m) ()
sinkWSText = CL.mapM_ $ wrapWS WS.sendTextData

-- | A @Sink@ for sending binary data to the user.
--
-- Since 0.1.0
sinkWSBinary :: (MonadIO m, WS.WebSocketsData a) => Consumer a (WebSocketsT m) ()
sinkWSBinary = CL.mapM_ $ wrapWS WS.sendBinaryData

race_ :: MonadBaseControl IO m => m a -> m b -> m ()
race_ x y = void $ race x y

