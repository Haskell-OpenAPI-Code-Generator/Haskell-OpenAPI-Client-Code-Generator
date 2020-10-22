{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}

-- | Utility functions for the OpenAPI code generator
module OpenAPI.Generate.Internal.Util
  ( haskellifyText,
    haskellifyName,
    haskellifyNameM,
    transformToModuleName,
    uppercaseFirstText,
    mapMaybeM,
    liftedAppend,
    joinWithPoint,
    joinWith,
  )
where

import qualified Control.Applicative as Applicative
import qualified Data.Char as Char
import Data.Text (Text)
import qualified Data.Text as T
import Language.Haskell.TH
import qualified OpenAPI.Generate.Monad as OAM
import qualified OpenAPI.Generate.OptParse as OAO

-- | Checks if the casing of a character can be changed.
-- This is required to ensure the functions 'Char.toUpper' and 'Char.toLower' actually do something.
isCasableAlpha :: Char -> Bool
isCasableAlpha x = Char.isLower (Char.toLower x) && Char.isUpper (Char.toUpper x)

isValidCharaterInSuffixExceptUnderscore :: Char -> Bool
isValidCharaterInSuffixExceptUnderscore x = isCasableAlpha x || Char.isDigit x || x == '\''

removeIllegalLeadingCharacters :: String -> String
removeIllegalLeadingCharacters (x : xs) | not (isCasableAlpha x) = removeIllegalLeadingCharacters xs
removeIllegalLeadingCharacters x = x

generateNameForEmptyIdentifier :: Text -> String -> String
generateNameForEmptyIdentifier originalName "" = "identifier" <> show (T.foldr ((+) . Char.ord) 0 originalName)
generateNameForEmptyIdentifier _ name = name

-- | Transform an identifier to ensure it is a valid Haskell identifier
-- Additionally, this function applies style settings according to the need of the consumer.
haskellifyText ::
  -- | Should the identifier be transformed to CamelCase?
  Bool ->
  -- | Should the first character of the identifier be uppercase?
  Bool ->
  -- | The identifier to transform
  Text ->
  -- | The resulting identifier
  String
haskellifyText convertToCamelCase startWithUppercase name =
  let casefn = if startWithUppercase then Char.toUpper else Char.toLower
      replaceChar '.' = '\''
      replaceChar '\'' = '\''
      replaceChar a = if isValidCharaterInSuffixExceptUnderscore a then a else '_'
      caseFirstCharCorrectly (x : xs) = casefn x : xs
      caseFirstCharCorrectly x = x
      nameWithoutSpecialChars a = replaceChar <$> a
      toCamelCase (x : y : xs) | not (isValidCharaterInSuffixExceptUnderscore x) && isCasableAlpha y = Char.toUpper y : toCamelCase xs
      toCamelCase (x : xs) = x : toCamelCase xs
      toCamelCase xs = xs
      replaceReservedWord "case" = "case'"
      replaceReservedWord "class" = "class'"
      replaceReservedWord "data" = "data'"
      replaceReservedWord "default" = "default'"
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
      replaceReservedWord "module" = "module'"
      replaceReservedWord "newtype" = "newtype'"
      replaceReservedWord "of" = "of'"
      replaceReservedWord "then" = "then'"
      replaceReservedWord "type" = "type'"
      replaceReservedWord "where" = "where'"
      replaceReservedWord "Configuration" = "Configuration'"
      replaceReservedWord "MonadHTTP" = "MonadHTTP'"
      replaceReservedWord "StringifyModel" = "StringifyModel'"
      replaceReservedWord "SecurityScheme" = "SecurityScheme'"
      replaceReservedWord "AnonymousSecurityScheme" = "AnonymousSecurityScheme'"
      replaceReservedWord "JsonByteString" = "JsonByteString'"
      replaceReservedWord "JsonDateTime" = "JsonDateTime'"
      replaceReservedWord "RequestBodyEncoding" = "RequestBodyEncoding'"
      replaceReservedWord a = a
      replacePlus ('+' : rest) = "Plus" <> replacePlus rest
      replacePlus (x : xs) = x : replacePlus xs
      replacePlus a = a
   in replaceReservedWord $
        caseFirstCharCorrectly $
          generateNameForEmptyIdentifier name $
            removeIllegalLeadingCharacters $
              (if convertToCamelCase then toCamelCase else id) $
                nameWithoutSpecialChars $
                  replacePlus $
                    T.unpack name

-- | The same as 'haskellifyText' but transform the result to a 'Name'
haskellifyName :: Bool -> Bool -> Text -> Name
haskellifyName convertToCamelCase startWithUppercase name = mkName $ haskellifyText convertToCamelCase startWithUppercase name

-- | 'OAM.Generator' version of 'haskellifyName'
haskellifyNameM :: Bool -> Text -> OAM.Generator Name
haskellifyNameM startWithUppercase name = do
  flags <- OAM.getFlags
  pure $ haskellifyName (OAO.flagConvertToCamelCase flags) startWithUppercase name

-- | Transform a module name to ensure it is valid for file names
transformToModuleName :: Text -> Text
transformToModuleName name =
  let toCamelCase (x : y : xs) | not (isValidCharaterInSuffixExceptUnderscore x) && isCasableAlpha y = Char.toUpper y : toCamelCase xs
      toCamelCase ('\'' : y : xs) | isCasableAlpha y = '\'' : Char.toUpper y : toCamelCase xs
      toCamelCase (x : xs) = x : toCamelCase xs
      toCamelCase xs = xs
   in T.pack $
        uppercaseFirst $
          generateNameForEmptyIdentifier name $
            removeIllegalLeadingCharacters $
              fmap
                ( \case
                    '\'' -> '_'
                    c -> c
                )
                $ toCamelCase $
                  T.unpack $
                    T.map
                      ( \case
                          '.' -> '\''
                          c | isValidCharaterInSuffixExceptUnderscore c -> c
                          _ -> '_'
                      )
                      name

uppercaseFirst :: String -> String
uppercaseFirst (x : xs) = Char.toUpper x : xs
uppercaseFirst x = x

-- | Uppercase the first character of a 'Text'
uppercaseFirstText :: Text -> Text
uppercaseFirstText = T.pack . uppercaseFirst . T.unpack

-- | Concat a list of strings with a point
--
-- >>> joinWithPoint ["a", "b", "c"]
-- "a.b.c"
joinWithPoint :: [String] -> String
joinWithPoint = joinWith "."

-- | Concat a list of values separated by an other value
joinWith :: Monoid a => a -> [a] -> a
joinWith _ [] = mempty
joinWith separator xs =
  foldr1
    ( \part1 part2 -> part1 <> separator <> part2
    )
    xs

-- | A version of 'Data.Maybe.mapMaybe' that works with a monadic predicate.
-- from https://hackage.haskell.org/package/extra-1.7.1/docs/src/Control.Monad.Extra.html#mapMaybeM copied
mapMaybeM :: Monad m => (a -> m (Maybe b)) -> [a] -> m [b]
mapMaybeM op = foldr f (pure [])
  where
    f x xs = do x' <- op x; case x' of { Nothing -> xs; Just x'' -> do { xs' <- xs; pure $ x'' : xs' } }

-- | Lifted version of '<>' which can be used with 'Semigroup's inside 'Applicative's
liftedAppend :: (Applicative f, Semigroup a) => f a -> f a -> f a
liftedAppend = Applicative.liftA2 (<>)
