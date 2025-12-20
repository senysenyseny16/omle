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
import Omle.Parser.Common (Parser, lexeme, symbol, quotes)
import Text.Megaparsec
import Text.Megaparsec.Char (string', alphaNumChar) -- string' is case-insensitive
import qualified Text.Megaparsec.Char.Lexer as L

parseScalar :: Parser YamlScalar
parseScalar = try parseFloat <|> try parseInt <|> try parseBool <|> parseNull <|> parseString

parseFloat :: Parser YamlScalar
parseFloat = YamlFloat <$> lexeme L.float

parseInt :: Parser YamlScalar
parseInt = YamlInt <$> lexeme L.decimal

parseBool :: Parser YamlScalar
parseBool = YamlBool True  <$ choice (map (\s -> lexeme (string' s) <* boundary) trueVals)
  <|> YamlBool False <$ choice (map (\s -> lexeme (string' s) <* boundary) falseVals)
  where
    trueVals  = ["yes", "y", "true", "on"]
    falseVals = ["no", "n", "false", "off"]
    boundary  = notFollowedBy alphaNumChar
      -- Ensures a full boolean literal is consumed and prevents partial matches,
      -- e.g., "yesX" or "true123" will fail. 
      -- Note: order of alternatives still matters - longer literals (like "yes") 
      -- must come before shorter overlapping ones (like "y").

parseNull :: Parser YamlScalar
parseNull = YamlNull <$ (lexeme (string' "null") <|> symbol "~")

parseString :: Parser YamlScalar
parseString = YamlString <$> quotes (takeWhileP Nothing (/= '"'))