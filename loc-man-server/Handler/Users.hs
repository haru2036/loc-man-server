{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE DataKinds #-}

module Handler.Users
( loggedIn
)
where

import Web.Apiary
import Web.Apiary.Authenticate
import Web.Apiary.Session.ClientSession
import Web.Apiary.Database.Persist 
import Web.Apiary.Logger

loggedIn :: ApiaryT '[Persist, Logger, Auth, Web.Apiary.Session.ClientSession.Session OpenId IO] '[] IO IO ()
loggedIn = do
      authorized . action $ do
        auth <- param [key|auth|]
        case claimed auth of
          Just claim -> appendShowing claim
          Nothing -> appendShowing "hogehoge"
