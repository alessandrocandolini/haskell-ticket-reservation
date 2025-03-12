{-# LANGUAGE QuasiQuotes #-}

module ModelsSpec where

import Env (Error (EmptyError), parsePure)
import Models (
  AppConfig (AppConfig),
  parser,
 )
import Test.Hspec (Spec, describe, it, shouldBe)
import Test.QuickCheck.Property ()

spec :: Spec
spec = describe "Models" $ do
  describe "AppConfig parser" $ do
    it "can parse env variables when correctly set" $
      parsePure parser [("REDIS_HOST", "hello"), ("REDIS_PORT", "10")] `shouldBe` Right (AppConfig "hello" 10)

    it "can parse env variables when redis port is not set" $
      parsePure parser [("REDIS_HOST", "hello")] `shouldBe` Right (AppConfig "hello" 6379)

    it "uses default values when no env variable is set" $
      parsePure parser [] `shouldBe` Right (AppConfig "localhost" 6379)

    it "fails to parse empty env variables" $
      parsePure parser [("REDIS_HOST", ""), ("REDIS_PORT", "")] `shouldBe` Left [("REDIS_HOST", EmptyError), ("REDIS_PORT", EmptyError)]
