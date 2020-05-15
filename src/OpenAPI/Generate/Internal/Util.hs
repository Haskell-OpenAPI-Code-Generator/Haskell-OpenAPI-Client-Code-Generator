{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TemplateHaskell #-}

module OpenAPI.Generate.Internal.Util where

import Data.Char
import qualified Data.Int as I
import qualified Data.Scientific as S
import qualified Data.Text as T
import Data.Time.Calendar
import Data.Time.LocalTime
import Language.Haskell.TH
import Language.Haskell.TH.Syntax
import qualified OpenAPI.Common as OAC
import OpenAPI.Generate.Flags
import qualified OpenAPI.Generate.Flags as OAF
import qualified OpenAPI.Generate.Monad as OAM
import qualified OpenAPI.Generate.Types.Schema as OAS

haskellifyText :: Bool -> Bool -> T.Text -> String
haskellifyText convertToCamelCase startWithUppercase name =
  let casefn = if startWithUppercase then toUpper else toLower
      replaceChar '.' = '\''
      replaceChar '\'' = '\''
      replaceChar a = if isAlphaNum a then a else '_'
      caseFirstCharCorrectly (x : xs) = casefn x : xs
      caseFirstCharCorrectly x = x
      nameWithoutSpecialChars a = replaceChar <$> a
      toCamelCase (x : y : xs) | not (isAlphaNum x) && x /= '\'' && isAlpha y = toUpper y : toCamelCase xs
      toCamelCase (x : xs) = x : toCamelCase xs
      toCamelCase xs = xs
      replaceReservedWord "case" = "case'"
      replaceReservedWord "class" = "class'"
      replaceReservedWord "data" = "data'"
      replaceReservedWord "deriving" = "deriving'"
      replaceReservedWord "do" = "do'"
      replaceReservedWord "else" = "else'"
      replaceReservedWord "if" = "if'"
      replaceReservedWord "import" = "import'"
      replaceReservedWord "in" = "in'"
      replaceReservedWord "infix" = "infix'"
      replaceReservedWord "infixl" = "infixl'"
      replaceReservedWord "infixr" = "infixr'"
      replaceReservedWord "instance" = "instance'"
      replaceReservedWord "let" = "let'"
      replaceReservedWord "of" = "of'"
      replaceReservedWord "module" = "module'"
      replaceReservedWord "newtype" = "newtype'"
      replaceReservedWord "then" = "then'"
      replaceReservedWord "type" = "type'"
      replaceReservedWord "where" = "where'"
      replaceReservedWord ('_' : rest) = replaceReservedWord rest
      replaceReservedWord a = a
      replacePlus ('+' : rest) = "Plus" <> replacePlus rest
      replacePlus (x : xs) = x : replacePlus xs
      replacePlus a = a
   in replaceReservedWord
        $ caseFirstCharCorrectly
        $ (if convertToCamelCase then toCamelCase else id)
        $ nameWithoutSpecialChars
        $ replacePlus
        $ T.unpack name

haskellifyName :: Bool -> Bool -> T.Text -> Name
haskellifyName convertToCamelCase startWithUppercase name = mkName $ haskellifyText convertToCamelCase startWithUppercase name

haskellifyNameM :: Bool -> T.Text -> OAM.Generator Name
haskellifyNameM startWithUppercase name = do
  flags <- OAM.getFlags
  pure $ haskellifyName (OAF.optConvertToCamelCase flags) startWithUppercase name

transformToModuleName :: T.Text -> T.Text
transformToModuleName =
  let toCamelCase (x : y : xs) | not (isAlphaNum x) && isAlpha y = toUpper y : toCamelCase xs
      toCamelCase (x : xs) = x : toCamelCase xs
      toCamelCase xs = xs
      uppercaseFirst (x : xs) = toUpper x : xs
      uppercaseFirst x = x
   in T.pack . toCamelCase . uppercaseFirst . T.unpack . T.map (\c -> if isAlphaNum c then c else '_')

getSchemaType :: Flags -> OAS.SchemaObject -> Name
getSchemaType _ OAS.SchemaObject {type' = OAS.SchemaTypeInteger, format = Just "int32", ..} = ''I.Int32
getSchemaType _ OAS.SchemaObject {type' = OAS.SchemaTypeInteger, format = Just "int64", ..} = ''I.Int64
getSchemaType _ OAS.SchemaObject {type' = OAS.SchemaTypeInteger, ..} = ''Integer
getSchemaType Flags {optUseFloatWithArbitraryPrecision = True, ..} OAS.SchemaObject {type' = OAS.SchemaTypeNumber, ..} = ''S.Scientific
getSchemaType _ OAS.SchemaObject {type' = OAS.SchemaTypeNumber, format = Just "float", ..} = ''Float
getSchemaType _ OAS.SchemaObject {type' = OAS.SchemaTypeNumber, format = Just "double", ..} = ''Double
getSchemaType _ OAS.SchemaObject {type' = OAS.SchemaTypeNumber, ..} = ''Double
getSchemaType _ OAS.SchemaObject {type' = OAS.SchemaTypeString, format = Just "byte", ..} = ''OAC.JsonByteString
getSchemaType _ OAS.SchemaObject {type' = OAS.SchemaTypeString, format = Just "binary", ..} = ''OAC.JsonByteString
getSchemaType Flags {optUseDateTypesAsString = True, ..} OAS.SchemaObject {type' = OAS.SchemaTypeString, format = Just "date", ..} = ''Day
getSchemaType Flags {optUseDateTypesAsString = True, ..} OAS.SchemaObject {type' = OAS.SchemaTypeString, format = Just "date-time", ..} = ''OAC.JsonDateTime
getSchemaType _ OAS.SchemaObject {type' = OAS.SchemaTypeString, ..} = ''String
getSchemaType _ OAS.SchemaObject {type' = OAS.SchemaTypeBool, ..} = ''Bool
getSchemaType _ OAS.SchemaObject {..} = ''String

joinWithPoint :: [String] -> String
joinWithPoint [] = ""
joinWithPoint xs =
  foldr1
    ( \part1 part2 -> part1 <> "." <> part2
    )
    xs
