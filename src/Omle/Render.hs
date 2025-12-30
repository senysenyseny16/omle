{-# LANGUAGE OverloadedStrings #-}

module Omle.Render (renderScalar, renderValue) where

import Data.Text (Text)
import qualified Data.Text as T
import Omle.AST

{- | Render a YAML scalar value to Text.
Strings are quoted, numbers, booleans, and null are rendered literally.
-}
renderScalar :: YamlScalar -> Text
renderScalar (YamlString t) = "\"" <> t <> "\"" -- simple quoting
renderScalar (YamlInt i) = T.pack (show i)
renderScalar (YamlFloat f) = T.pack (show f)
renderScalar (YamlBool True) = "true"
renderScalar (YamlBool False) = "false"
renderScalar YamlNull = "null"

-- helpers for rendering the indented structures
indentStep :: Int
indentStep = 2

indentText :: Int -> Text
indentText n = T.replicate n " "

-- Render a YAML value to Text, with proper indentation.
-- Scalars are rendered inline, sequences and mappings as block structures.
renderValue :: Int -> YamlValue -> Text
renderValue _ (YamlScalar s) = renderScalar s
renderValue ind (YamlSequence xs) =
  T.intercalate "\n" $ map renderItem xs
 where
  renderItem v =
    case v of
      -- Scalars are rendered on the same line with '- '
      -- Sequences and mappings (nested structures) are rendered starting on a new line
      YamlScalar _ -> indentText ind <> "- " <> renderValue (ind + indentStep) v
      _ -> indentText ind <> "-\n" <> renderValue (ind + indentStep) v
renderValue ind (YamlMapping kvs) =
  T.intercalate "\n" $ map renderPair kvs
 where
  renderPair (k, v) =
    case v of
      -- Scalars are rendered on the same line with 'key: '
      -- Sequences and mappings (nested structures) are rendered starting on a new line
      YamlScalar _ -> indentText ind <> k <> ": " <> renderValue (ind + indentStep) v
      _ -> indentText ind <> k <> ":\n" <> renderValue (ind + indentStep) v
