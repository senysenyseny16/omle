{-# LANGUAGE OverloadedStrings #-}

module Omle.Parser.Constants (trueVals, falseVals, nullVals) where

import Data.Text (Text)

{- | True and false literals allowed as boolean values
(according to the YAML 1.2 Core Schema)
-}
trueVals :: [Text]
trueVals = ["true", "True", "TRUE"]

falseVals :: [Text]
falseVals = ["false", "False", "FALSE"]

-- | Null literals (YAML 1.2 Core Schema)
nullVals :: [Text]
nullVals = ["null", "Null", "NULL", "~"]