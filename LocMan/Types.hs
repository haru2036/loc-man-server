module LocMan.Types where
import Model
import Data.Map.Strict
import Data.Text
import Control.Concurrent.STM.TChan

-- | This module has non-persistent data types.
-- If the application stopped, data that has these data types will be deleted.

type LocationSessionId = Text
type UserLocationSessions = Map LocationSessionId UserLocationSession
data UserLocationSession = UserLocationSession 
                           { sessionUsers ::[User]
                           , sessionMasterChannel :: TChan UserLocationRecord
                           }
-- data UserLocationRecord = UserLocationRecord
type UserLocationRecord = Text
