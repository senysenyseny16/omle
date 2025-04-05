{-# LANGUAGE OverloadedStrings #-}

module Parser.YamlValueSpec (spec) where

import Omle.AST
import Omle.Parser.YamlValue (parseMapping, parseSequence)
import Test.Hspec
import Text.Megaparsec

spec :: Spec
spec = do
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
        "{key: {key1: [44, false]}}"
        `shouldBe` Right
          (YamlMapping [("key", YamlMapping [("key1", YamlSequence [YamlScalar (YamlInt 44), YamlScalar (YamlBool False)])])])
