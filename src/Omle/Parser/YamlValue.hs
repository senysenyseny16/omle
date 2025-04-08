{-# LANGUAGE TupleSections     #-}

module Omle.Parser.YamlValue
  ( parseYamlValue,
    parseSequence,
    parseMapping,
    parseKey,
  )
where

import Omle.AST
import Omle.Parser.Common (Parser, scn, braces, brackets, colon, comma, dash, parseKey)
import qualified Omle.Parser.YamlScalar as YamlScalar
import Text.Megaparsec
import Text.Megaparsec.Char (eol)
import qualified Text.Megaparsec.Char.Lexer as L

parseYamlValue :: Parser YamlValue
parseYamlValue = try parseScalar <|> try parseSequence <|> parseMapping
parseScalar :: Parser YamlValue
parseScalar = YamlScalar <$> YamlScalar.parseScalar

parseSequence :: Parser YamlValue
parseSequence = try parseFlowSequence <|> parseBlockSequence

parseMapping :: Parser YamlValue
parseMapping = try parseNestedMapping <|> try parseComplexMapping <|> try parseFlowMapping <|> parseBlockMapping

parseFlowSequence :: Parser YamlValue
parseFlowSequence = YamlSequence <$> brackets (parseYamlValue `sepBy` comma)

parseBlockSequence :: Parser YamlValue
parseBlockSequence = YamlSequence <$> some parseBlockSequenceItem

parseBlockSequenceItem :: Parser YamlValue
parseBlockSequenceItem = do
  _ <- dash
  parseYamlValue <* eol

parseFlowMapping :: Parser YamlValue
parseFlowMapping = YamlMapping <$> braces (parseFlowMappingItem `sepBy` comma)
  where
    parseFlowMappingItem = do
      key <- parseKey
      _ <- colon
      value <- parseYamlValue
      return (key, value)

parseBlockMapping :: Parser YamlValue
parseBlockMapping = YamlMapping <$> some parseBlockMappingItem
  where
    parseBlockMappingItem = do
      key <- parseKey
      _ <- colon
      val <- parseYamlValue <* eol
      return (key, val)
    {-
    parseNestedBlockMapping = L.nonIndented scn (L.indentBlock scn p)
      where
        p = do
          key <- parseKey
          _ <- colon
          return (L.IndentSome Nothing (return . (\x -> [(key, YamlSequence x)])) parseYamlValue)
    -}
parseComplexMapping :: Parser YamlValue
parseComplexMapping = YamlMapping <$> L.indentBlock scn p
  where
    p = do
      key <- parseKey
      _ <- colon
      return (L.IndentSome Nothing (return . (\x -> [(key, YamlSequence x)])) parseYamlValue)

parseNestedMapping :: Parser YamlValue
parseNestedMapping = YamlMapping <$> L.nonIndented scn (L.indentBlock scn p)
      where
        p = do
          key <- parseKey
          _ <- colon
          return (L.IndentSome Nothing (return . (\x -> [(key, YamlSequence x)])) parseComplexMapping)



