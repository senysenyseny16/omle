{-# LANGUAGE OverloadedStrings #-}

module Omle.Parser.YamlScalar
  ( parseScalar,
    parseFloat,
    parseInt,
    parseBool,
    parseNull,
    parseString,
  )
where

import Control.Applicative hiding (many, some)
import Omle.AST
import Omle.Parser.Common (Parser, lexeme, symbol)
import Text.Megaparsec
import Text.Megaparsec.Char (char, string, string') -- string is case-insensitive
import qualified Text.Megaparsec.Char.Lexer as L

parseScalar :: Parser YamlScalar
parseScalar = try parseFloat <|> try parseInt <|> try parseBool <|> parseNull <|> parseString

parseFloat :: Parser YamlScalar
parseFloat = YamlFloat <$> lexeme L.float

parseInt :: Parser YamlScalar
parseInt = YamlInt <$> lexeme L.decimal

parseBool :: Parser YamlScalar
parseBool = (lexeme (string "true") >> return (YamlBool True)) <|> (lexeme (string "false") >> return (YamlBool False))

parseNull :: Parser YamlScalar
parseNull = YamlNull <$ (lexeme (string' "null") <|> symbol "~")

parseString :: Parser YamlScalar
parseString = YamlString <$> (lexeme (char '"') *> takeWhileP Nothing (/= '"') <* lexeme (char '"'))