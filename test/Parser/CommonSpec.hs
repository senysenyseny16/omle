{-# LANGUAGE OverloadedStrings #-}

module Parser.CommonSpec (spec) where

import Omle.Parser.Common (scn, sc)
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
