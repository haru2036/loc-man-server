{-# LANGUAGE OverloadedStrings#-}
module Main where
import Network.WebSockets(runServer, acceptRequest, PendingConnection)
import LocMan.WebSockets(runSockets, joinSession)
import LocMan.Types
import LocMan.Model
import Database.Persist.Sqlite
import Control.Concurrent.STM.TVar(newTVar, TVar)
import Control.Concurrent.STM(atomically)
import Data.Text ()
import Data.Map (empty)

main :: IO ()
main = do
  _ <- runSqlite ":memory:" $ do
        runMigration migrateAll
  appStatus <- atomically $ newTVar empty
  let usr = User "stub" Nothing
  sess <- atomically $ do
      joinSession usr appStatus  "1"
  runServer "127.0.0.1" 3000 $ serverApp usr sess

serverApp :: User -> TVar (UserLocationSession User) -> PendingConnection -> IO () 
serverApp usr status pd = runSockets usr status =<< acceptRequest pd
