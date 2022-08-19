{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}

module OpenAPI.Generate.DocSpec where

import Data.List
import GHC.Generics
import Language.Haskell.TH
import Language.Haskell.TH.PprLib
import OpenAPI.Generate.Doc
import Test.Hspec
import Test.QuickCheck
import Test.Validity

newtype MultiLineString = MultiLineString String
  deriving (Eq, Show, Generic)

instance Validity MultiLineString

instance GenValid MultiLineString where
  genValid =
    MultiLineString
      <$> genListOf
        ( frequency [(9, genValid :: Gen Char), (1, pure '\n')]
        )

spec :: Spec
spec = do
  describe "sideBySide" $ do
    it "should place equally long docs" $
      show
        ( sideBySide
            ( text "a" $$ text "c"
            )
            (text "b" $$ text "d")
        )
        `shouldBe` show (text "a b" $$ text "c d")
    it "should indent if right doc is longer" $
      show
        ( sideBySide
            ( text "a" $$ text "c"
            )
            (text "b" $$ text "d" $$ text "e")
        )
        `shouldBe` show (text "a b" $$ text "c d" $$ text "e")
    it "should not indent if left doc is longer" $
      show
        ( sideBySide
            ( text "a" $$ text "c" $$ text "e"
            )
            (text "b" $$ text "d")
        )
        `shouldBe` show (text "a b" $$ text "c d" $$ text "e")
    it "should have the length of the longer document" $ do
      let numberOfLinesOfDoc = length . dropWhileEnd null . lines . show
      forAllValid $ \(MultiLineString doc1, MultiLineString doc2) ->
        numberOfLinesOfDoc
          ( sideBySide
              (text doc1)
              (text doc2)
          )
          == max (numberOfLinesOfDoc $ text doc1) (numberOfLinesOfDoc $ text doc2)
  describe "generateHaddockComment" $
    it "should place every item on a new line and respect newline characters" $
      show (generateHaddockComment ["Line 1", "Line 2", "", "Line 3\nLine 4"])
        `shouldBe` init
          ( unlines
              [ "-- | Line 1",
                "-- Line 2",
                "-- ",
                "-- Line 3",
                "-- Line 4"
              ]
          )
  describe "sideComments" $
    it "should convert every item to a side comment and replace newlines with spaces" $
      show (sideComments ["Line 1", "Line 2", "", "Line 3\nLine 4"])
        `shouldBe` init
          ( unlines
              [ "-- ^ Line 1",
                "-- ^ Line 2",
                "-- ^ ",
                "-- ^ Line 3 Line 4"
              ]
          )
  describe "appendDoc" $
    it "should append two docs" $
      do
        content <- runQ $ pure (text "a") `appendDoc` pure (text "b")
        show content `shouldBe` "a\nb"
  describe "breakOnTokens" $
    it "place a line feed before the tokens and add an indentation" $
      show
        ( breakOnTokens [",", "}"] $
            text $
              unlines
                [ "foo = {",
                  "  a = 123, b = 321,",
                  "  c = A",
                  "  } deriving Foo"
                ]
        )
        `shouldBe` unlines
          [ "foo = { a = 123",
            "  , b = 321",
            "  , c = A",
            "  } deriving Foo"
          ]
  describe "zipCodeAndComments" $
    it "should intertwine code and comments" $
      show
        ( zipCodeAndComments
            [ "foo = {",
              "  a = 123",
              "  , b = 321",
              "  , c = A ",
              "  } deriving Foo"
            ]
            [ "a is foo",
              "b is bar\nbut remember the line feed",
              "c is A"
            ]
        )
        `shouldBe` init
          ( unlines
              [ "foo = {",
                "  -- | a is foo",
                "  a = 123",
                "  -- | b is bar",
                "  -- but remember the line feed",
                "  , b = 321",
                "  -- | c is A",
                "  , c = A ",
                "  } deriving Foo"
              ]
          )
