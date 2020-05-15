{-# LANGUAGE DeriveGeneric #-}

module OpenAPI.Generate.DocSpec where

import GHC.Generics
import Language.Haskell.TH.PprLib
import OpenAPI.Generate.Doc
import OpenAPI.Generate.Util
import Test.Hspec
import Test.QuickCheck
import Test.Validity

newtype MultiLineString = MultiLineString String
  deriving (Eq, Show, Generic)

instance Validity MultiLineString

instance GenUnchecked MultiLineString where
  genUnchecked =
    MultiLineString
      <$> genListOf
        ( frequency [(9, genUnchecked :: Gen Char), (1, pure '\n')]
        )

instance GenValid MultiLineString

spec :: Spec
spec =
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
        `shouldBe` show (text "a b" $$ text "c d" $$ text "  e")
    it "should not indent if left doc is longer" $
      show
        ( sideBySide
            ( text "a" $$ text "c" $$ text "e"
            )
            (text "b" $$ text "d")
        )
        `shouldBe` show (text "a b" $$ text "c d" $$ text "e")
    it "should have the length of the longer document" $ do
      let numberOfLinesOfDoc = length . splitOn '\n' . show
      forAllValid $ \(MultiLineString doc1, MultiLineString doc2) ->
        numberOfLinesOfDoc
          ( sideBySide
              (text doc1)
              (text doc2)
          )
          == max (numberOfLinesOfDoc $ text doc1) (numberOfLinesOfDoc $ text doc2)
