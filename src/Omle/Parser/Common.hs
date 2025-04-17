{-# LANGUAGE OverloadedStrings #-}

module Omle.Parser.Common
  ( Parser,
    lineComment,
    scn,
    lexeme,
    symbol,
    comma,
    colon,
    dash,
    braces,
    brackets,
    parseKey,
    matchIndent
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

lexeme :: Parser a -> Parser a
lexeme = L.lexeme scn

symbol :: Text -> Parser Text
symbol = L.symbol scn

comma :: Parser Text
comma = symbol ","

colon :: Parser Text
colon = L.symbol sc ":" -- not consuming newline after colon, as it defines whether there is nesting or not

dash :: Parser Text
dash = symbol "-"

braces :: Parser a -> Parser a
braces = between (symbol "{") (symbol "}")

brackets :: Parser a -> Parser a
brackets = between (symbol "[") (symbol "]")

parseKey :: Parser Text
parseKey = lexeme (takeWhile1P Nothing isAlphaNum)

-- Check that current indentation matches the expected one, or fail with error
matchIndent :: Pos -> Parser ()
matchIndent expected = do
  actual <- L.indentLevel
  if actual == expected
    then pure ()
    else 
      fail $ "Incorrect indentation: expected " ++ show expected ++ ", but got " ++ show actual
