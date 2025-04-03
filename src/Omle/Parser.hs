{-# LANGUAGE OverloadedStrings #-}

module Omle.Parser (parseInt, parseBool, parseScalar) where
    
import Omle.AST
import Text.Megaparsec
import Text.Megaparsec.Char (string)
import qualified Text.Megaparsec.Char.Lexer as L
import Data.Text (Text)
import Data.Void

type Parser = Parsec Void Text

parseInt :: Parser YamlScalar
parseInt = YamlInt <$> L.decimal

parseBool :: Parser YamlScalar
parseBool = (string "true" >> return (YamlBool True)) <|> (string "false" >> return (YamlBool False))

parseScalar :: Parser YamlScalar
parseScalar = try parseInt <|> try parseBool
