{-# LANGUAGE OverloadedStrings #-}

module Parser.YamlValueSpec (spec) where

import Omle.AST
import Omle.Parser.YamlValue (parseMapping, parseSequence, parseYamlValue)
import Test.Hspec
import Text.Megaparsec
import qualified Data.Text as T

spec :: Spec
spec = do
  describe "parseSequence" $ do
    it "parses nested list of scalars" $ do
      parse
        parseSequence
        ""
        "[32.456 ,  false, [7, hello, true],  1, NULL]"
        `shouldBe` Right
          ( YamlSequence
              [ YamlScalar (YamlFloat 32.456)
              , YamlScalar (YamlBool False)
              , YamlSequence
                  [ YamlScalar (YamlInt 7)
                  , YamlScalar (YamlString "hello")
                  , YamlScalar (YamlBool True)
                  ]
              , YamlScalar (YamlInt 1)
              , YamlScalar YamlNull
              ]
          )

  describe "parseMapping" $ do
    it "parses nested mappings with mixed scalar types" $ do
      parse
        parseMapping
        ""
        "{key: {key1: [44, false, hello, \"world\", null]}}"
        `shouldBe` Right
          (YamlMapping
            [ ("key"
              , YamlMapping
                  [ ("key1"
                    , YamlSequence
                        [ YamlScalar (YamlInt 44)
                        , YamlScalar (YamlBool False)
                        , YamlScalar (YamlString "hello")
                        , YamlScalar (YamlString "world")
                        , YamlScalar YamlNull
                        ]
                    )
                  ]
              )
            ])

  describe "parseYamlValue" $ do
    it "parses structures with indentation including strings" $ do
      parse
        parseYamlValue
        ""
        (T.unlines
          [ "k1 :"
          , "  k11 : "
          , "    - 1"
          , "    - 2"
          , "    - hello"        -- plain string
          , "    - \"world\""    -- quoted string
          , "  k12 :"
          , "    k121 : [true, 1, foo, ~]"  -- flow sequence with plain string and null
          ])
        `shouldBe` Right
          (YamlMapping
            [ ("k1"
              , YamlMapping
                [ ("k11", YamlSequence
                    [ YamlScalar (YamlInt 1)
                    , YamlScalar (YamlInt 2)
                    , YamlScalar (YamlString "hello")
                    , YamlScalar (YamlString "world")
                    ])
                , ("k12", YamlMapping
                    [ ("k121", YamlSequence
                        [ YamlScalar (YamlBool True)
                        , YamlScalar (YamlInt 1)
                        , YamlScalar (YamlString "foo")
                        , YamlScalar YamlNull
                        ])
                    ])
                ])
            ])