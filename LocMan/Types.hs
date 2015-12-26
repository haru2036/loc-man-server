{-# LANGUAGE QuasiQuotes, TemplateHaskell #-}

module LocMan.Types where

import Model
import Data.Map.Strict
import Data.Text
import qualified Data.Text.Lazy as TL
import Control.Concurrent.STM.TMChan
import Control.Concurrent.STM.TVar
import Control.Lens
import Data.Aeson.TH
import ClassyPrelude

-- | This module has non-persistent data types.
-- If the application stopped, data that has these data types will be deleted.

type LocationSessionId = Text

type Meter = Float

data GeoLocation = GeoLocation
              { _latitude :: Double
              , _longitude :: Double
              , _altitude :: Double
              , _error :: Meter
              }

$(deriveJSON defaultOptions ''GeoLocation)
$(makeLenses ''GeoLocation)

data JUser = JUser
            { _uid :: Text 
            , _name :: Text
            }
$(deriveJSON defaultOptions ''JUser)

data SessionEvent = Joined JUser | Exited JUser | UpdateLocation GeoLocation

$(deriveJSON defaultOptions ''SessionEvent)


data UserLocationSession = UserLocationSession 
                           { _sessionUsers :: [User]
                           , _sessionMasterChannel :: TMChan TL.Text
                           }

$(makeLenses ''UserLocationSession)

type AppStates = Map LocationSessionId (TVar UserLocationSession)
