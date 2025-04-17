{-# LANGUAGE OverloadedStrings #-}

module Omle.Parser.YamlScalar
  ( parseScalar,
    parseFloat,
    parseInt,
    parseBool,
    parseString
  )
where

import Control.Applicative hiding (many, some)
import Omle.AST
import Omle.Parser.Common (Parser, lexeme)
import Text.Megaparsec
import Text.Megaparsec.Char (string, char)
import qualified Text.Megaparsec.Char.Lexer as L

parseScalar :: Parser YamlScalar
parseScalar = try parseFloat <|> try parseInt <|> try parseBool <|> parseString

parseFloat :: Parser YamlScalar
parseFloat = YamlFloat <$> lexeme L.float

parseInt :: Parser YamlScalar
parseInt = YamlInt <$> lexeme L.decimal

parseBool :: Parser YamlScalar
parseBool = (lexeme (string "true") >> return (YamlBool True)) <|> (lexeme (string "false") >> return (YamlBool False))

parseString :: Parser YamlScalar
parseString = YamlString <$> (lexeme (char '"') *> takeWhileP Nothing (/= '"') <* lexeme (char '"'))