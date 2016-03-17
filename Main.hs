{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE DataKinds #-}

import Web.Apiary
import Web.Apiary.WebSockets
import Network.Wai.Handler.Warp
import qualified Data.Text as T
import Network.Routing.Dict(get)
import Control.Concurrent
import Language.Haskell.TH
import System.FilePath
import System.Directory
import Locman.WebSockets
import Control.Concurrent.STM.TVar(newTVar)
import Control.Concurrent.STM(atomically)
import Model
import Data.Map(empty)

main :: IO ()
main = do 
    setCurrentDirectory $(location >>= stringE . takeDirectory . loc_filename)
    runApiary (run 3000) def $ do
        [capture|/i::Int|] . webSockets $ servApp . get [key|i|]
        root $ actionWithWebSockets (const $ servApp 0) (file "websockets.html" Nothing)

servApp :: Int -> PendingConnection -> IO ()
servApp st pc = do
    c <- acceptRequest pc
    let usr = User "hoge" Nothing
    sess <- atomically $ do
      appStatus <- newTVar empty
      joinSession usr appStatus  "1"
    runSockets c usr sess 
