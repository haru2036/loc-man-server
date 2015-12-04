{-# LANGUAGE QuasiQuotes       #-}
{-# LANGUAGE TemplateHaskell   #-}

module LocMan.Types where

import Model
import Data.Map.Strict
import Data.Text
import qualified Data.Text.Lazy as TL
import Control.Concurrent.STM.TChan
import Control.Concurrent.STM.TVar
import Control.Lens

-- | This module has non-persistent data types.
-- If the application stopped, data that has these data types will be deleted.

type LocationSessionId = Text
type AppStates = Map LocationSessionId (TVar UserLocationSession)
data UserLocationSession = UserLocationSession 
                           { _sessionUsers :: [User]
                           , _sessionMasterChannel :: TChan UserLocationRecord
                           }
-- data UserLocationRecord = UserLocationRecord
type UserLocationRecord = TL.Text

$(makeLenses ''UserLocationSession)
