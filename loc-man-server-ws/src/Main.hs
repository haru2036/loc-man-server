{-# LANGUAGE OverloadedStrings#-}
module Main where
import Network.WebSockets(runServer, acceptRequest, PendingConnection)
import LocMan.WebSockets(runSockets, joinSession)
import LocMan.Types
import LocMan.Model
import Database.Persist.Postgresql
import Database.Persist
import Control.Monad.Logger
import Control.Monad.IO.Class(liftIO)
import Control.Concurrent.STM.TVar(newTVar, TVar)
import Control.Concurrent.STM(atomically)
import Data.Text ()
import Data.Map (empty)

connStr = "host=iphoge dbname=test user=postgres password=hoge port=32768"

main :: IO ()
main = do
  runStderrLoggingT $ withPostgresqlPool connStr 10 $ \pool -> liftIO $ do
         flip runSqlPersistMPool pool $ do
            runMigration migrateAll
  print "db connection started"
  appStatus <- atomically $ newTVar empty
  let usr = User "stub" Nothing
  sess <- atomically $ do
      joinSession usr appStatus  "1"
  runServer "127.0.0.1" 3000 $ serverApp usr sess

serverApp :: User -> TVar (UserLocationSession User) -> PendingConnection -> IO () 
serverApp usr status pd = runSockets usr status =<< acceptRequest pd
