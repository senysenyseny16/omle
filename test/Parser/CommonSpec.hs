{-# LANGUAGE OverloadedStrings #-}

module Parser.CommonSpec (spec) where

import Omle.Parser.Common (sc)
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
