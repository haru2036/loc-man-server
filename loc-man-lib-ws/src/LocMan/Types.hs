{-# LANGUAGE QuasiQuotes, TemplateHaskell, Rank2Types #-}

module LocMan.Types where

import Data.Text(Text)
import qualified Data.Text.Lazy as TL
import Control.Concurrent.STM.TMChan
import Control.Concurrent.STM.TVar
import Control.Monad.IO.Class
import Control.Lens
import Data.Aeson.TH
import Data.Map

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

class (Eq a) => ModelUser a where
  toJUser :: a -> JUser
  fromJUser :: MonadIO m => JUser -> m a

$(deriveJSON defaultOptions{ fieldLabelModifier = drop 1} ''JUser)

data SessionEvent = Joined | Exited | UpdateLocation GeoLocation

data UserSessionEvent = UserSessionEvent { 
                          author :: JUser
                        , event :: SessionEvent
                        }

$(deriveJSON defaultOptions { fieldLabelModifier = drop 1} ''SessionEvent)

$(deriveJSON defaultOptions { fieldLabelModifier = drop 0} ''UserSessionEvent)


data UserLocationSession a = UserLocationSession 
                           { sessionUsers ::  ModelUser a => [a]
                           , sessionMasterChannel :: TMChan UserSessionEvent
                           }

type AppStatus a = Map LocationSessionId (TVar (UserLocationSession a))

