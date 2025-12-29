module Omle.Parser.YamlValue (
  parseYamlValue,
  parseSequence,
  parseMapping,
  parseKey,
)
where

import Data.Text (Text)
import Omle.AST
import Omle.Parser.Common (
  Parser,
  braces,
  brackets,
  colon,
  comma,
  dash,
  matchIndent,
  parseKey,
  scn,
 )
import qualified Omle.Parser.YamlScalar as YamlScalar
import Text.Megaparsec
import Text.Megaparsec.Char (newline)
import qualified Text.Megaparsec.Char.Lexer as L

parseYamlValue :: Parser YamlValue
parseYamlValue = try parseSequence <|> try parseMapping <|> parseScalar

parseScalar :: Parser YamlValue
parseScalar = YamlScalar <$> YamlScalar.parseScalar

parseSequence :: Parser YamlValue
parseSequence = try parseFlowSequence <|> parseBlockSequence

parseMapping :: Parser YamlValue
parseMapping = try parseFlowMapping <|> parseBlockMapping

parseSeqOrMap :: Parser YamlValue
parseSeqOrMap = try parseSequence <|> parseMapping

parseFlowSequence :: Parser YamlValue
parseFlowSequence = YamlSequence <$> brackets (parseYamlValue `sepBy` comma)

parseBlockSequence :: Parser YamlValue
parseBlockSequence = do
  indent <- L.indentLevel
  YamlSequence <$> some (parseBlockSequenceItemAt indent)
 where
  parseBlockSequenceItemAt expectedIndent = do
    matchIndent expectedIndent
    parseBlockSequenceItem

parseBlockSequenceItem :: Parser YamlValue
parseBlockSequenceItem = do
  indent <- L.indentLevel
  _ <- dash
  -- lookAhead to look without actually consuiming the input
  nestedMapAhead <- optional (lookAhead (try (parseKey <* colon <* newline)))
  case nestedMapAhead of
    Just _ -> YamlMapping . (: []) <$> parseBlockMappingItem (Just indent)
    -- item can be either a nested mapping or some other value,
    -- in case of nested mapping the indentation of the leading
    -- dash should be taken into account
    Nothing -> parseYamlValue

parseFlowMapping :: Parser YamlValue
parseFlowMapping = YamlMapping <$> braces (parseKeyValue `sepBy` comma)
 where
  parseKeyValue = do
    key <- parseKey <* colon <* scn
    value <- parseYamlValue
    return (key, value)

parseBlockMapping :: Parser YamlValue
parseBlockMapping = do
  indent <- L.indentLevel
  YamlMapping <$> some (parseBlockMappingItemAt indent)
 where
  parseBlockMappingItemAt expectedIndent = do
    matchIndent expectedIndent
    parseBlockMappingItem Nothing

parseBlockMappingItem :: Maybe Pos -> Parser (Text, YamlValue)
parseBlockMappingItem topIndent = do
  indent <- case topIndent of
    Just x -> return x
    Nothing -> L.indentLevel
  key <- parseKey <* colon
  -- check if there is an inline scalar or flow collection
  inlineVal <-
    optional (try parseFlowSequence <|> try parseFlowMapping <|> parseScalar)
  case inlineVal of
    Just value -> return (key, value) -- simple key-value case
    Nothing -> do
      -- case of a nested structure
      _ <- scn
      nextIndent <- L.indentLevel
      if nextIndent > indent
        then do
          value <- parseSeqOrMap -- parsing nested block
          return (key, value)
        else
          fail "Expected nested block after key"