module Omle.Parser.YamlValue
  ( parseYamlValue,
    parseSequence,
    parseMapping,
    parseKey,
  )
where

import Omle.AST
import Omle.Parser.Common (Parser, braces, brackets, colon, comma, lexeme)
import qualified Omle.Parser.YamlScalar as YamlScalar
import Text.Megaparsec
import Text.Megaparsec.Char (alphaNumChar)

parseYamlValue :: Parser YamlValue
parseYamlValue = choice [parseScalar, parseSequence, parseMapping]

parseScalar :: Parser YamlValue
parseScalar = YamlScalar <$> YamlScalar.parseScalar

parseSequence :: Parser YamlValue
parseSequence = YamlSequence <$> brackets (parseYamlValue `sepBy` comma)

parseMapping :: Parser YamlValue
parseMapping = YamlMapping <$> braces (parseKeyValue `sepBy` comma)

parseKey :: Parser [Char]
parseKey = lexeme (some alphaNumChar)

parseKeyValue :: Parser (String, YamlValue)
parseKeyValue = do
  key <- try parseKey
  _ <- colon
  value <- parseYamlValue
  return (key, value)
