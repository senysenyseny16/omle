{-# LANGUAGE OverloadedStrings #-}

module Omle.Parser.Common
  ( Parser,
    lineComment,
    scn,
    sc,
    lexeme,
    symbol,
    comma,
    colon,
    dash,
    braces,
    brackets,
    parseKey
  )
where

import Control.Applicative hiding (many, some)
import Control.Monad (void)
import Data.Text (Text)
import Data.Char (isAlphaNum)
import Data.Void
import Text.Megaparsec
import Text.Megaparsec.Char (space1)
import qualified Text.Megaparsec.Char.Lexer as L

type Parser = Parsec Void Text

lineComment :: Parser ()
lineComment = L.skipLineComment "#"

scn :: Parser ()
scn = L.space space1 lineComment empty

sc :: Parser ()
sc = L.space (void $ takeWhile1P Nothing f) lineComment empty
  where
    f x = x == ' ' || x == '\t'
--sc :: Parser () -- space consumer
--sc = L.space space1 (L.skipLineComment "#") empty

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

parseKey :: Parser Text
parseKey = lexeme (takeWhile1P Nothing isAlphaNum)