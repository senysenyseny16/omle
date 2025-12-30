{-# LANGUAGE OverloadedStrings #-}

module Omle.Render (renderScalar) where

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
