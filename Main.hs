{-# LANGUAGE OverloadedStrings #-}

import Web.Scotty
import Model
import Locman.WebSocket

main :: IO ()
main = scotty 3000 $ do
  get "/:word" $ do
    beam <- param "word"
    html $ mconcat ["<h1>Scotty, ", beam, " me up!</h1>"]
  get "/ws/" $ do
    runSocket $ User "hoge" "piyo"
