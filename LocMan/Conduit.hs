{-# LANGUAGE BangPatterns #-}
module LocMan.Conduit where

import Conduit
import Data.Aeson
import Data.Aeson.Types()
import Data.ByteString.Lazy
import Data.Text.Lazy
import Debug.Trace
import ClassyPrelude hiding (ByteString, Text)

traceConduit :: (MonadIO m, Show a) => Conduit a m a
traceConduit = do
  awaitForever $ \x -> do
    !a <- return $ trace ("passed value is :" ++ show x) x
    yield a

decodeConduit :: (MonadIO m, FromJSON a) => Conduit ByteString m (Either String a)
decodeConduit = awaitForever $ yield . eitherDecode

encodeConduit :: (MonadIO m, ToJSON a) => Conduit a m Text
encodeConduit = awaitForever $ yield . decodeUtf8 . encode 

errorReportConduit :: (MonadIO m, Show b) => Conduit (Either b a) m a
errorReportConduit = awaitForever $ \x -> case x of
                                          Right a -> yield a
                                          Left b -> error (show b)

toByteStringConduit :: (MonadIO m) => Conduit Text m ByteString 
toByteStringConduit = awaitForever $ yield . encodeUtf8

