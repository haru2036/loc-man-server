{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE DataKinds #-}

import Web.Apiary
import Web.Apiary.WebSockets
import Web.Apiary.Database.Persist
import Network.Wai.Handler.Warp
import qualified Data.Text as T
import Network.Routing.Dict(get)
import Control.Concurrent
import Language.Haskell.TH
import System.FilePath
import System.Directory
import Locman.WebSockets
import LocMan.Types
import Control.Concurrent.STM.TVar(newTVar, TVar)
import Control.Concurrent.STM(atomically)
import Data.Map(empty)
import Model


main :: IO ()
main = do 
    appStatus <- atomically $ newTVar empty
    setCurrentDirectory $(location >>= stringE . takeDirectory . loc_filename)
    runApiary (run 3000) def $ do
        [capture|/i::Int|] . webSockets $ servApp appStatus . get [key|i|]
        root $ actionWithWebSockets (const $ servApp appStatus 0) (file "websockets.html" Nothing)

servApp :: TVar AppStatus -> Int -> PendingConnection -> IO ()
servApp appStatus st pc = do
    c <- acceptRequest pc
    let usr = User "hoge" Nothing
    sess <- atomically $ do
      joinSession usr appStatus  "1"
    runSockets c usr sess 
