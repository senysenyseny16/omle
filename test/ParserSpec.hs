{-# LANGUAGE OverloadedStrings #-}

module ParserSpec (spec) where

import Omle.AST
import Omle.Parser (parseBool, parseFloat, parseInt, parseScalar, parseSequence, parseMapping, sc)
import Test.Hspec
import Test.Hspec.Megaparsec
import Text.Megaparsec

spec :: Spec
spec = do
  describe "spaceConsumer" $ do
    it "skips spaces" $ do
      parse sc "" `shouldSucceedOn` "      "
    it "skips comment line" $ do
      parse sc "" `shouldSucceedOn` "# a comment line"

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

  describe "parseSequence" $ do
    it "parses nested list of scalars" $ do
      parse
        parseSequence
        ""
        "[32.456 ,  false, [7, true],  1 ]"
        `shouldBe` Right
          ( YamlSequence
              [ YamlScalar (YamlFloat 32.456),
                YamlScalar (YamlBool False),
                YamlSequence [YamlScalar (YamlInt 7), YamlScalar (YamlBool True)],
                YamlScalar (YamlInt 1)
              ]
          )

  describe "parseMapping" $ do
    it "parses nested mappings" $ do
      parse
        parseMapping
        ""
        "key  : key1:[44, false]"
        `shouldBe` Right
          ( YamlMapping ("key", 
              YamlMapping ("key1",
                YamlSequence 
                [ YamlScalar (YamlInt 44)
                , YamlScalar (YamlBool False)
                ]
              )
            )
          )
              
