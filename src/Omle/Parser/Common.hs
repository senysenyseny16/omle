{-# LANGUAGE OverloadedStrings #-}

module Omle.Parser.Common
  ( Parser,
    sc,
    lexeme,
    symbol,
    comma,
    colon,
    dash,
    braces,
    brackets,
    parseKey,
  )
where

import Control.Applicative hiding (many, some)
import Data.Text (Text)
import Data.Void
import Text.Megaparsec
import Text.Megaparsec.Char (alphaNumChar, space1)
import qualified Text.Megaparsec.Char.Lexer as L

type Parser = Parsec Void Text

sc :: Parser () -- space consumer
sc = L.space space1 (L.skipLineComment "#") empty

lexeme :: Parser a -> Parser a
lexeme = L.lexeme sc

symbol :: Text -> Parser Text
symbol = L.symbol sc

comma :: Parser Text
comma = symbol ","

colon :: Parser Text
colon = symbol ":"

dash :: Parser Text
dash = symbol "-"

braces :: Parser a -> Parser a
braces = between (symbol "{") (symbol "}")

brackets :: Parser a -> Parser a
brackets = between (symbol "[") (symbol "]")

parseKey :: Parser [Char]
parseKey = lexeme (some alphaNumChar)
