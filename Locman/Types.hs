{-# LANGUAGE QuasiQuotes, TemplateHaskell #-}

module LocMan.Types where

import Model
import Data.Map.Strict(Map)
import Data.Text(Text)
import qualified Data.Text.Lazy as TL
import Control.Concurrent.STM.TMChan
import Control.Concurrent.STM.TVar
import Control.Lens
import Data.Aeson.TH

-- | This module has non-persistent data types.
-- If the application stopped, data that has these data types will be deleted.

type LocationSessionId = Text

type Meter = Float

data GeoLocation = GeoLocation
              { _latitude :: Double
              , _longitude :: Double
              , _altitude :: Double
              , _accuracy:: Meter
              }

$(deriveJSON defaultOptions { fieldLabelModifier = drop 1} ''GeoLocation)
$(makeLenses ''GeoLocation)

data JUser = JUser
            { _uid :: Text 
            , _name :: Text
            } deriving (Eq)

-- stub
userToJUser :: User -> JUser
userToJUser usr = JUser (userIdent usr) (userIdent usr) 

$(deriveJSON defaultOptions{ fieldLabelModifier = drop 1} ''JUser)

data SessionEvent = Joined | Exited | UpdateLocation GeoLocation

data UserSessionEvent = UserSessionEvent { 
                          author :: JUser
                        , event :: SessionEvent
                        }

$(deriveJSON defaultOptions { fieldLabelModifier = drop 1} ''SessionEvent)

$(deriveJSON defaultOptions { fieldLabelModifier = drop 0} ''UserSessionEvent)


data UserLocationSession = UserLocationSession 
                           { _sessionUsers :: [User]
                           , _sessionMasterChannel :: TMChan UserSessionEvent
                           }

$(makeLenses ''UserLocationSession)

type AppStatus = Map LocationSessionId (TVar UserLocationSession)

