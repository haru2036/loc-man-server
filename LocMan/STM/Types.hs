module LocMan.STM.Types where

-- | This module has non-persistent data types.
-- If the application stopped, data that has these data types will be deleted.
type UserLocationSessions = [UserLocationSession]
data UserLocationSession = UserLocationSession
