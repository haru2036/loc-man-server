{-# LANGUAGE BangPatterns #-}
module LocMan.Conduit where

import Conduit
import Data.Aeson
import Data.Aeson.Types
import Data.ByteString.Lazy
import Debug.Trace
import ClassyPrelude hiding (ByteString)

traceConduit :: (MonadIO m, Show a) => Conduit a m a
traceConduit = do
  awaitForever $ \x -> do
    !a <- return $ trace ("passed value is :" ++ show x) x
    yield a

decodeConduit :: (MonadIO m, FromJSON a) => Conduit ByteString m (Either String a)
decodeConduit = awaitForever $ yield . eitherDecode

encodeConduit :: (MonadIO m, ToJSON a) => Conduit a m ByteString 
encodeConduit = awaitForever $ yield . encode 
