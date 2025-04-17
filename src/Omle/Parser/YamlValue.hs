module Omle.Parser.YamlValue
  ( parseYamlValue,
    parseSequence,
    parseMapping,
    parseKey
  )
where
import Data.Text (Text)
import Omle.AST
import Omle.Parser.Common (Parser, scn, braces, brackets, colon, comma, dash, parseKey, matchIndent)
import qualified Omle.Parser.YamlScalar as YamlScalar
import Text.Megaparsec
import qualified Text.Megaparsec.Char.Lexer as L

parseYamlValue :: Parser YamlValue
parseYamlValue = try parseScalar <|> try parseSequence <|> parseMapping

parseScalar :: Parser YamlValue
parseScalar = YamlScalar <$> YamlScalar.parseScalar

parseSequence :: Parser YamlValue
parseSequence = try parseFlowSequence <|> parseBlockSequence

parseMapping :: Parser YamlValue
parseMapping = try parseFlowMapping <|> parseBlockMapping -- try parseNestedMapping <|> try parseComplexMapping <|> 

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
  (YamlMapping . (:[]) <$> try (parseBlockMappingItem (Just indent))) <|> parseYamlValue -- item can be either a nested mapping or some other value, 
                                                                                         -- in case of nested mapping the indentation of the leading 
                                                                                         -- dash should be taken into account

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
    Just x  -> return x
    Nothing -> L.indentLevel
  key <- parseKey <* colon
  inlineVal <- optional parseScalar -- check if the value is inline (scalar)
  case inlineVal of
    Just value -> return (key, value) -- simple key-value(scalar) case
    Nothing -> do                     -- case of a nested structure
      _ <- scn
      nextIndent <- L.indentLevel
      if nextIndent > indent
        then do
          value <- parseSeqOrMap  -- parsing nested block
          return (key, value)
        else
          fail "Expected nested block after key"