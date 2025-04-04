{-# LANGUAGE OverloadedStrings #-}

module Omle.Parser (sc, parseFloat, parseInt, parseBool, parseScalar, parseScalars) where

import Data.Text (Text)
import Data.Void
import Omle.AST
import Text.Megaparsec
import Text.Megaparsec.Char (space1, string)
import qualified Text.Megaparsec.Char.Lexer as L

type Parser = Parsec Void Text

sc :: Parser () -- space consumer
sc =
  L.space
    space1
    (L.skipLineComment "#")
    empty

lexeme :: Parser a -> Parser a
lexeme = L.lexeme sc

symbol :: Text -> Parser Text
symbol    = L.symbol sc
parens    = between (symbol "(") (symbol ")")
braces    = between (symbol "{") (symbol "}")
angles    = between (symbol "<") (symbol ">")
brackets  = between (symbol "[") (symbol "]")
semicolon = symbol ";"
comma     = symbol ","
colon     = symbol ":"
dot       = symbol "."

parseFloat :: Parser YamlScalar
parseFloat = YamlFloat <$> lexeme L.float

parseInt :: Parser YamlScalar
parseInt = YamlInt <$> lexeme L.decimal

parseBool :: Parser YamlScalar
parseBool = (lexeme (string "true") >> return (YamlBool True)) <|> (lexeme (string "false") >> return (YamlBool False))

parseScalar :: Parser YamlScalar
parseScalar = try parseFloat <|> try parseInt <|> try parseBool

parseScalar' :: Parser YamlValue
parseScalar' = YamlScalar <$> parseScalar

parseScalars :: Parser YamlValue
parseScalars = YamlSequence <$> brackets (parseScalar' `sepBy` comma)
