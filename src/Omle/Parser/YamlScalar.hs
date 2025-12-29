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
import Omle.Parser.Common (Parser, lexeme, symbol, exactLiteral, quotes)
import Text.Megaparsec
import qualified Text.Megaparsec.Char.Lexer as L

parseScalar :: Parser YamlScalar
parseScalar = try parseFloat <|> try parseInt <|> try parseBool <|> parseNull <|> parseString

parseFloat :: Parser YamlScalar
parseFloat = YamlFloat <$> lexeme L.float

parseInt :: Parser YamlScalar
parseInt = YamlInt <$> lexeme L.decimal

parseBool :: Parser YamlScalar
parseBool = YamlBool True  <$ choice (map exactLiteral trueVals)
  <|> YamlBool False <$ choice (map exactLiteral falseVals)
  where
    trueVals  = ["true", "True", "TRUE"]
    falseVals = ["false", "False", "FALSE"]

parseNull :: Parser YamlScalar
parseNull = YamlNull <$ (choice (map exactLiteral nullVals) <|> symbol "~")
  where
    nullVals = ["null", "Null", "NULL"]

parseString :: Parser YamlScalar
parseString = YamlString <$> quotes (takeWhileP Nothing (/= '"'))