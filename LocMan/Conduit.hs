module LocMan.Conduit where

import Conduit
import Data.Aeson
import Debug.Trace

traceConduit :: (MonadIO m, Show a) => Conduit a m a
traceConduit = do
  awaitForever $ \x -> do
    !a <- return $ trace ("passed value is :" ++ show x) x
    yield a

parseConduit :: (MonadIO m, FromJSON a) => Conduit Object m a
parseConduit = _

jsonConduit :: (MonadIO m, ToJSON a) => Conduit a m JSON
parseConduit = _
