{-# LANGUAGE QuasiQuotes #-}

module ServerSpec where

import Server (appWithConfig)
import Test.Hspec (Spec, describe, it, shouldBe)
import Test.Hspec.QuickCheck (prop)
import Test.Hspec.Wai (ResponseMatcher (matchStatus), get, shouldRespondWith, with)
import Test.Hspec.Wai.JSON (json)
import Test.QuickCheck.Property ()
import Models (AppConfig(..))

testConfig :: AppConfig
testConfig = AppConfig "localhost" 6379

wrongConfig :: AppConfig
wrongConfig = AppConfig "localhost" 6380

spec :: Spec
spec = describe "Simple test" $ do
  it "example-based unit test" $
    1 `shouldBe` 1

  prop "property-based unit test" $
    \l -> reverse (reverse l) == (l :: [Int])

  with (appWithConfig testConfig) $ do
    describe "GET /healthcheck/" $ do
      it "responds with ok status" $ do
        let response = [json|{"data":{"status":"ok"}}|]
        get "/healthcheck/" `shouldRespondWith` response{matchStatus = 200}

  with (appWithConfig wrongConfig) $ do
    describe "GET /healthcheck/" $ do
      it "fails to connect to redis" $ do
        let response = [json|{"err":{"message":"cannot connect to the database"}}|]
        get "/healthcheck/" `shouldRespondWith` response{matchStatus = 500}

