module Omle.AST (YamlValue (..), YamlScalar (..)) where

import Data.Text (Text)

data YamlValue
  = YamlScalar YamlScalar
  | YamlSequence [YamlValue]
  | YamlMapping [(Text, YamlValue)]
  deriving (Eq, Show)

data YamlScalar
  = YamlString String
  | YamlInt Int
  | YamlFloat Float
  | YamlBool Bool
  | YamlNull
  deriving (Eq, Show)
