{-# LANGUAGE OverloadedStrings #-}

module Parser.CommonSpec (spec) where

import Data.Either (isLeft)
import Omle.Parser.Common (exactLiteral, sc, scn)
import Test.Hspec
import Test.Hspec.Megaparsec
import Text.Megaparsec
import Text.Megaparsec.Char (newline)

spec :: Spec
spec = do
  describe "spaceConsumer scn" $ do
    it "skips spaces, newlines, tabs" $ do
      parse scn "" `shouldSucceedOn` "     \t \n"
    it "skips line comment" $ do
      parse scn "" `shouldSucceedOn` "# a comment line"
  describe "spaceConsumer sc" $ do
    it "skips spaces and tabs" $ do
      parse sc "" `shouldSucceedOn` "     \t "
    it "doesn't consume newlines" $ do
      parse (sc *> newline) "" `shouldSucceedOn` "    \n"
    it "skips line comment" $ do
      parse sc "" `shouldSucceedOn` "# a comment line"
  describe "exactLiteral parser" $ do
    it "parses the given exact literal" $ do
      parse (exactLiteral "something") "" "something"
        `shouldBe` Right "something"
    it "fails when followed by alphanumeric characters" $ do
      parse (exactLiteral "something") "" "somethingx12"
        `shouldSatisfy` isLeft
    it "consumes trailing whitespace" $ do
      parse (exactLiteral "something") "" "something   "
        `shouldBe` Right "something"
