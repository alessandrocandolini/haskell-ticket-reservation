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

  with app $ do
    describe "GET /health/live/" $ do
      it "responds with ok status" $ do
        let response = [json|{"status":"ok"}|]
        get "/health/live/" `shouldRespondWith` response{matchStatus = 200}

    describe "GET /health/startup/" $ do
      it "responds with ok status" $ do
        let response = [json|{"status":"ok"}|]
        get "/health/startup/" `shouldRespondWith` response{matchStatus = 200}

    describe "GET /health/ready/" $ do
      it "responds with ok status" $ do
        let response = [json|{"status":"ok"}|]
        get "/health/ready/" `shouldRespondWith` response{matchStatus = 200}
