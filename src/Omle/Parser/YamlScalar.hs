{-# LANGUAGE OverloadedStrings #-}

module Omle.Parser.YamlScalar
  ( parseScalar,
    parseFloat,
    parseInt,
    parseBool,
  )
where

import Control.Applicative hiding (many, some)
import Omle.AST
import Omle.Parser.Common (Parser, lexeme)
import Text.Megaparsec
import Text.Megaparsec.Char (string)
import qualified Text.Megaparsec.Char.Lexer as L

parseScalar :: Parser YamlScalar
parseScalar = try parseFloat <|> try parseInt <|> try parseBool

parseFloat :: Parser YamlScalar
parseFloat = YamlFloat <$> lexeme L.float

parseInt :: Parser YamlScalar
parseInt = YamlInt <$> lexeme L.decimal

parseBool :: Parser YamlScalar
parseBool = (lexeme (string "true") >> return (YamlBool True)) <|> (lexeme (string "false") >> return (YamlBool False))
