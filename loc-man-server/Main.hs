{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE DataKinds #-}

import Web.Apiary
import Web.Apiary.Authenticate
import Web.Apiary.Session.ClientSession
import Web.Apiary.Cookie
import Web.Apiary.Database.Persist 
import Web.Apiary.Logger
import Database.Persist.Postgresql(withPostgresqlPool)
import Network.Wai.Handler.Warp
import qualified Data.Text as T
import Network.Routing.Dict(get)
import Control.Concurrent
import Language.Haskell.TH
import System.FilePath
import System.Directory
import LocMan.Types
import LocMan.Model
import Control.Concurrent.STM.TVar(newTVar, TVar)
import Control.Concurrent.STM(atomically)
import Data.Map(empty)
import Handler.Users


connStr = "host=192.168.99.100 dbname=test user=postgres password=hogepero port=32768"

sc :: ClientSessionConfig
sc = def { csCookiePath = Just "/", csCookieSecure = False }

ac ::AuthConfig
ac = def { authSuccessPage = "/loggedin", authUrl = "http://localhost:3000", providers = [("hatena", hatena),
                                                           ("yahoo", yahoo)]}
hatena :: Provider
hatena = Provider "http://www.hatena.ne.jp/openid/server"  Nothing []

yahoo :: Provider
yahoo = Provider "http://me.yahoo.com/" Nothing []

main :: IO ()
main = do 
  setCurrentDirectory $(location >>= stringE . takeDirectory . loc_filename)
  runApiaryWith (run 3000) (initClientSession pOpenId sc +> initAuth ac +> initLogger def +> initPersistPool (withPostgresqlPool connStr 10) migrateAll) def $ do
    authHandler
    root . method GET $ do
      authorized . action $ do

        contentType "text/html"

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

    [capture|/loggedin|] . method GET $ loggedIn

    [capture|/websock|] . method GET . action $ file "websockets.html" Nothing
    [capture|/logout|] . method GET . action $ do
      authLogout
      setCookie def { setCookieName = "message", setCookieValue = "logout done, bye." }
      redirect "/"

