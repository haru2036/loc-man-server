{-# LANGUAGE BangPatterns #-}
module LocMan.Conduit.Common where

import LocMan.Types
import Conduit
import Data.Aeson
import Data.Aeson.Types()
import Data.ByteString.Lazy
import Data.Text.Lazy
import Debug.Trace
import qualified ClassyPrelude as CP
import Control.Monad.Trans.Maybe

traceConduit :: (MonadIO m, Show a) => Conduit a m a
traceConduit = do
  awaitForever $ \x -> do
    !a <- return $ trace ("passed value is :" ++ show x) x
    yield a

decodeConduit :: (MonadIO m, FromJSON a) => Conduit ByteString m (Either String a)
decodeConduit = awaitForever $ yield . eitherDecode

encodeConduit :: (MonadIO m, ToJSON a) => Conduit a m Text
encodeConduit = awaitForever $ yield . CP.decodeUtf8 . encode 

errorReportConduit :: (MonadIO m, Show b) => Conduit (Either b a) m a
errorReportConduit = awaitForever $ \x -> case x of
                                          Right a -> yield a
                                          Left b -> error (show b)

toByteStringConduit :: (MonadIO m) => Conduit Text m ByteString 
toByteStringConduit = awaitForever $ yield . CP.encodeUtf8

addAuthorConduit :: (MonadIO m) => JUser-> Conduit SessionEvent m UserSessionEvent
addAuthorConduit u = awaitForever $ yield . UserSessionEvent u

dropWithUserConduit :: (MonadIO m) => JUser -> Conduit UserSessionEvent m UserSessionEvent
dropWithUserConduit u = do
  _ <- runMaybeT $ do
   us <- MaybeT await
   case author us of
    x 
      | x ==  u -> return ()
      | otherwise -> lift $ yield us >> return ()
  dropWithUserConduit u
  
