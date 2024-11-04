{-# LANGUAGE OverloadedStrings #-}

module OpenAPI.Generate.Internal.UtilSpec where

import qualified Data.Char as Char
import Data.GenValidity.Text ()
import Data.Text (Text)
import qualified Data.Text as T
import Data.Validity.Text ()
import OpenAPI.Generate.Internal.Util
import Test.Hspec
import Test.Validity

-- See https://www.haskell.org/onlinereport/lexemes.html § 2.4
isValidVarId :: Text -> Bool
isValidVarId "case" = False
isValidVarId "class" = False
isValidVarId "data" = False
isValidVarId "default" = False
isValidVarId "deriving" = False
isValidVarId "do" = False
isValidVarId "else" = False
isValidVarId "if" = False
isValidVarId "import" = False
isValidVarId "in" = False
isValidVarId "infix" = False
isValidVarId "infixl" = False
isValidVarId "infixr" = False
isValidVarId "instance" = False
isValidVarId "let" = False
isValidVarId "module" = False
isValidVarId "newtype" = False
isValidVarId "of" = False
isValidVarId "then" = False
isValidVarId "type" = False
isValidVarId "where" = False
isValidVarId other = case T.unpack other of
  [] -> False
  (x : xs) -> isValidSmall x && isValidSuffix xs

isValidConId :: Text -> Bool
isValidConId other = case T.unpack other of
  [] -> False
  (x : xs) -> isValidLarge x && isValidSuffix xs

isValidSmall :: Char -> Bool
isValidSmall x = x == '_' || Char.isLower x

isValidLarge :: Char -> Bool
isValidLarge = Char.isUpper

isValidSuffix :: String -> Bool
isValidSuffix = all (\x -> isValidSmall x || isValidLarge x || Char.isDigit x || x == '\'')

spec :: Spec
spec = do
  describe "haskellifyText" $ do
    it "uppercase without CamelCase" $
      forAllValid $
        isValidConId . haskellifyText False True
    it "uppercase with CamelCase" $
      forAllValid $
        isValidConId . haskellifyText True True
    it "lowercase without CamelCase" $
      forAllValid $
        isValidVarId . haskellifyText False False
    it "lowercase with CamelCase" $
      forAllValid $
        isValidVarId . haskellifyText True False
  describe "transformToModuleName" $
    it "should be valid module name" $
      forAllValid $
        isValidConId . transformToModuleName
