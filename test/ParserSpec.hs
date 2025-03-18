module ParserSpec (spec) where

import Omle.AST (YamlScalar (YamlBool, YamlInt))
import Omle.Parser (parseBool, parseInt, parseScalar)
import Test.Hspec
import Text.Megaparsec

spec :: Spec
spec = do
  describe "parseInt" $ do
    it "parses integer" $ do
      parse parseInt "" "80" `shouldBe` Right (YamlInt 80)

  describe "parseBool" $ do
    it "parses true" $ do
      parse parseBool "" "true" `shouldBe` Right (YamlBool True)
    it "parses false" $ do
      parse parseBool "" "false" `shouldBe` Right (YamlBool False)

  describe "parseScalar" $ do
    it "parses integer" $ do
      parse parseScalar "" "1234" `shouldBe` Right (YamlInt 1234)
    it "parses bool" $ do
      parse parseScalar "" "false" `shouldBe` Right (YamlBool False)
