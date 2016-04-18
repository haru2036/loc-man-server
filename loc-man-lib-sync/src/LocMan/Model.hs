{-# LANGUAGE EmptyDataDecls             #-}
{-# LANGUAGE FlexibleContexts           #-}
{-# LANGUAGE GADTs                      #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses      #-}
{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE QuasiQuotes                #-}
{-# LANGUAGE TemplateHaskell            #-}
{-# LANGUAGE TypeFamilies               #-}
module LocMan.Model where


import Database.Persist.TH
import Data.Text
import LocMan.Types

share [mkPersist sqlSettings, mkMigrate "migrateAll"] [persistLowerCase|
User
    ident Text
    password Text Maybe
    UniqueUser ident
    deriving Eq

Session
    ident Int
    userList [User]
    UniqueSession ident
    deriving Eq
|]

instance ModelUser User where
  toJUser usr = JUser ident' ident'
    where
      ident' = userIdent usr
  fromJUser _ = return $ User "stub" Nothing
