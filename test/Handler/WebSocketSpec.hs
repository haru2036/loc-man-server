module Handler.WebSocketSpec (spec) where

import TestImport
import Handler.WebSocket

spec :: Spec
spec = withApp $ do

    describe "joinSession" $ do
        let uua = User "aaa" Nothing
        let uub = User "bbb" Nothing
        a <- atomically $ joinSession uua app "a"
        b <- atomically $ joinSession uub app "a"
        it a `shouldBe` b

