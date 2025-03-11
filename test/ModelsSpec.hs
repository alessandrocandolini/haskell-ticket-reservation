{-# LANGUAGE QuasiQuotes #-}

module ModelsSpec where

import Env (Error (EmptyError), parsePure)
import Models (
  ApplicationConfig (ApplicationConfig),
  applicationConfigEnvParser,
 )
import Test.Hspec (Spec, describe, it, shouldBe)
import Test.QuickCheck.Property ()

spec :: Spec
spec = describe "Models" $ do
  describe "ApplicationConfig parser" $ do
    it "can parse env variables when correctly set" $
      parsePure applicationConfigEnvParser [("REDIS_HOST", "hello"), ("REDIS_PORT", "10")] `shouldBe` Right (ApplicationConfig "hello" 10)

    it "can parse env variables when redis port is not set" $
      parsePure applicationConfigEnvParser [("REDIS_HOST", "hello")] `shouldBe` Right (ApplicationConfig "hello" 6379)

    it "uses default values when no env variable is set" $
      parsePure applicationConfigEnvParser [] `shouldBe` Right (ApplicationConfig "localhost" 6379)

    it "failt to parse empty env variables" $
      parsePure applicationConfigEnvParser [("REDIS_HOST", ""), ("REDIS_PORT", "")] `shouldBe` Left [("REDIS_HOST", EmptyError), ("REDIS_PORT", EmptyError)]
