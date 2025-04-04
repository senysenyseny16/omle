module Omle.AST (YamlValue (..), YamlScalar (..)) where

data YamlValue
  = YamlScalar YamlScalar
  | YamlSequence [YamlValue]
  | YamlMapping (String, YamlValue)
  deriving (Eq, Show)

data YamlScalar
  = YamlString String
  | YamlInt Int
  | YamlFloat Float
  | YamlBool Bool
  | YamlNull
  deriving (Eq, Show)
