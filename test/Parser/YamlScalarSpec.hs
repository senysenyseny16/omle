{-# LANGUAGE OverloadedStrings #-}

module Parser.YamlScalarSpec (spec) where

import Omle.AST
import Omle.Parser.YamlScalar (parseFloat, parseInt, parseBool, parseNull, parseScalar, parseString)
import Test.Hspec
import Text.Megaparsec
import Data.Either (isLeft)


spec :: Spec
spec = do
  describe "parseFloat" $ do
    it "parses float" $ do
      parse parseFloat "" "12.34 " `shouldBe` Right (YamlFloat 12.34)

  describe "parseInt" $ do
    it "parses integer" $ do
      parse parseInt "" "80 " `shouldBe` Right (YamlInt 80)

  describe "parseBool" $ do
    it "parses true" $
      parse parseBool "" "True " `shouldBe` Right (YamlBool True)
    it "parses false" $
      parse parseBool "" "FALSE " `shouldBe` Right (YamlBool False)
    it "rejects partial match true123" $
      parse parseBool "" "true123" `shouldSatisfy` isLeft

  describe "parseNull" $ do
    it "parses null" $ do
      parse parseNull "" "Null" `shouldBe` Right YamlNull
    it "parses ~ (null)" $ do
      parse parseNull "" "~" `shouldBe` Right YamlNull

  describe "parseString" $ do 
    it "parses string" $ do
      parse parseString "" "\"A string. \"" `shouldBe` Right (YamlString "A string. ")

  describe "parseScalar" $ do
    it "parses float" $ do
      parse parseScalar "" "12.34 " `shouldBe` Right (YamlFloat 12.34)
    it "parses integer" $ do
      parse parseScalar "" "1234 " `shouldBe` Right (YamlInt 1234)
    it "parses bool" $ do
      parse parseScalar "" "false " `shouldBe` Right (YamlBool False)
    it "parses null" $ do
      parse parseNull "" "NULL" `shouldBe` Right YamlNull
    it "parses string" $ do
      parse parseString "" "\"A string. \"" `shouldBe` Right (YamlString "A string. ")
