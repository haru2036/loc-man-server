{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE DataKinds #-}

import Web.Apiary
import Web.Apiary.WebSockets
import Web.Apiary.Authenticate
import Web.Apiary.Session.ClientSession
import Web.Apiary.Cookie
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


sc :: ClientSessionConfig
sc = def { csCookiePath = Just "/", csCookieSecure = False }

main :: IO ()
main = do 
  appStatus <- atomically $ newTVar empty
  setCurrentDirectory $(location >>= stringE . takeDirectory . loc_filename)
  runApiaryWith (run 3000) (initClientSession pOpenId sc +> initAuth def) def $ do
    authHandler
    root . method GET $ do
      authorized . action $ do

        contentType "text/html"

        bytes "your id: "
        appendShowing =<< param [key|auth|]
        appendBytes " \n<a href=\"/logout\">logout</a>"
      cookie [key|message|] (pOption pByteString) . action $ do
        contentType "text/html"

        reset
        maybe (return ()) (\m -> mapM_ appendBytes ["<h1>", m, "</h1>"]) =<< param [key|message|]

        authRoutes >>= mapM_ (\(n,r) -> do
        mapM_ appendBytes ["<div><a href=\"", r, "\">"]
        appendText n
        appendBytes "</a></div>")

        deleteCookie "message"

    [capture|/websock/i::Int|] . webSockets $ servApp appStatus . get [key|i|]
    actionWithWebSockets (const $ servApp appStatus 0) (file "websockets.html" Nothing)
    [capture|/websock|] . method GET . action $ file "websockets.html" Nothing
    [capture|/logout|] . method GET . action $ do
      authLogout
      setCookie def { setCookieName = "message", setCookieValue = "logout done, bye." }
      redirect "/"


servApp :: TVar AppStatus -> Int -> PendingConnection -> IO ()
servApp appStatus st pc = do
    c <- acceptRequest pc
    let usr = User "hoge" Nothing
    sess <- atomically $ do
      joinSession usr appStatus  "1"
    runSockets c usr sess 
