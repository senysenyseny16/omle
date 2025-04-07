module Omle.Parser.YamlValue
  ( parseYamlValue,
    parseSequence,
    parseMapping,
    parseKey,
  )
where

import Omle.AST
import Omle.Parser.Common (Parser, braces, brackets, colon, comma, dash, parseKey)
import qualified Omle.Parser.YamlScalar as YamlScalar
import Text.Megaparsec

-- import qualified Text.Megaparsec.Char.Lexer as L

parseYamlValue :: Parser YamlValue
parseYamlValue = try parseScalar <|> try parseSequence <|> parseMapping

parseScalar :: Parser YamlValue
parseScalar = YamlScalar <$> YamlScalar.parseScalar

parseSequence :: Parser YamlValue
parseSequence = try parseFlowSequence <|> parseBlockSequence

parseMapping :: Parser YamlValue
parseMapping = try parseFlowMapping <|> parseBlockMapping

parseFlowSequence :: Parser YamlValue
parseFlowSequence = YamlSequence <$> brackets (parseYamlValue `sepBy` comma)

parseBlockSequence :: Parser YamlValue
parseBlockSequence = YamlSequence <$> some parseBlockSequenceItem
  where
    parseBlockSequenceItem = do
      -- _ <- L.indentGuard sc GT pos1
      _ <- dash
      parseYamlValue

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
      -- _ <- L.indentGuard sc GT pos1
      key <- parseKey
      _ <- colon
      val <- parseYamlValue
      return (key, val)
