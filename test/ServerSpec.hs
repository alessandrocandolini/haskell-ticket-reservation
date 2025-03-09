{-# LANGUAGE QuasiQuotes #-}

module ServerSpec where

import Server (app)
import Test.Hspec (Spec, describe, it, shouldBe)
import Test.Hspec.QuickCheck (prop)
import Test.Hspec.Wai (ResponseMatcher (matchStatus), get, shouldRespondWith, with)
import Test.Hspec.Wai.JSON (json)
import Test.QuickCheck.Property ()

spec :: Spec
spec = describe "Simple test" $ do
  it "example-based unit test" $
    1 `shouldBe` 1

  prop "property-based unit test" $
    \l -> reverse (reverse l) == (l :: [Int])

  with (return app) $ do
    describe "GET /status" $ do
      it "responds with ok status" $ do
        let response = [json|{"status":"ok"}|]
        get "/status" `shouldRespondWith` response{matchStatus = 200}
