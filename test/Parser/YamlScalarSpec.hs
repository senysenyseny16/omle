{-# LANGUAGE OverloadedStrings #-}

module Parser.YamlScalarSpec (spec) where

import Omle.AST
import Omle.Parser.YamlScalar (parseBool, parseFloat, parseInt, parseScalar)
import Test.Hspec
import Text.Megaparsec

spec :: Spec
spec = do
  describe "parseFloat" $ do
    it "parses float" $ do
      parse parseFloat "" "12.34 " `shouldBe` Right (YamlFloat 12.34)

  describe "parseInt" $ do
    it "parses integer" $ do
      parse parseInt "" "80 " `shouldBe` Right (YamlInt 80)

  describe "parseBool" $ do
    it "parses true" $ do
      parse parseBool "" "true " `shouldBe` Right (YamlBool True)
    it "parses false" $ do
      parse parseBool "" "false " `shouldBe` Right (YamlBool False)

  describe "parseScalar" $ do
    it "parses float" $ do
      parse parseScalar "" "12.34 " `shouldBe` Right (YamlFloat 12.34)
    it "parses integer" $ do
      parse parseScalar "" "1234 " `shouldBe` Right (YamlInt 1234)
    it "parses bool" $ do
      parse parseScalar "" "false " `shouldBe` Right (YamlBool False)
