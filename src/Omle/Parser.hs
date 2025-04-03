{-# LANGUAGE OverloadedStrings #-}

module Omle.Parser (sc, parseFloat, parseInt, parseBool, parseScalar) where
    
import Omle.AST
import Text.Megaparsec
import Text.Megaparsec.Char (string, space1)
import qualified Text.Megaparsec.Char.Lexer as L
import Data.Text (Text)
import Data.Void

type Parser = Parsec Void Text

sc :: Parser () -- space consumer
sc = L.space 
    space1 
    (L.skipLineComment "#") 
    empty

parseFloat :: Parser YamlScalar
parseFloat = YamlFloat <$> L.float

parseInt :: Parser YamlScalar
parseInt = YamlInt <$> L.decimal

parseBool :: Parser YamlScalar
parseBool = (string "true" >> return (YamlBool True)) <|> (string "false" >> return (YamlBool False))

parseScalar :: Parser YamlScalar
parseScalar = try parseFloat <|> try parseInt <|> try parseBool
