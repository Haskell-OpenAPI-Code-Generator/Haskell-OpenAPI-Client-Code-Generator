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
    splitOn,
    joinWithPoint,
    joinWith,
  )
where

import qualified Control.Applicative as Applicative
import qualified Data.Char as Char
import Data.Text (Text)
import qualified Data.Text as T
import Language.Haskell.TH
import qualified OpenAPI.Generate.Flags as OAF
import qualified OpenAPI.Generate.Monad as OAM

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
      replaceChar a = if Char.isAlphaNum a then a else '_'
      caseFirstCharCorrectly (x : xs) = casefn x : xs
      caseFirstCharCorrectly x = x
      nameWithoutSpecialChars a = replaceChar <$> a
      toCamelCase (x : y : xs) | not (Char.isAlphaNum x) && x /= '\'' && Char.isAlpha y = Char.toUpper y : toCamelCase xs
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

-- | The same as 'haskellifyText' but transform the result to a 'Name'
haskellifyName :: Bool -> Bool -> Text -> Name
haskellifyName convertToCamelCase startWithUppercase name = mkName $ haskellifyText convertToCamelCase startWithUppercase name

-- | 'OAM.Generator' version of 'haskellifyName'
haskellifyNameM :: Bool -> Text -> OAM.Generator Name
haskellifyNameM startWithUppercase name = do
  flags <- OAM.getFlags
  pure $ haskellifyName (OAF.optConvertToCamelCase flags) startWithUppercase name

-- | Transform a module name to ensure it is valid for file names
transformToModuleName :: Text -> Text
transformToModuleName =
  let toCamelCase (x : y : xs) | not (Char.isAlphaNum x) && Char.isAlpha y = Char.toUpper y : toCamelCase xs
      toCamelCase (x : xs) = x : toCamelCase xs
      toCamelCase xs = xs
   in T.pack . toCamelCase . uppercaseFirst . T.unpack . T.map (\c -> if Char.isAlphaNum c then c else '_')

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

-- | Split a list on on a given element
splitOn :: Eq a => a -> [a] -> [[a]]
splitOn x =
  foldr
    ( \element (currentAcc : acc) ->
        if element == x
          then [] : currentAcc : acc
          else (element : currentAcc) : acc
    )
    [[]]

-- | A version of 'Data.Maybe.mapMaybe' that works with a monadic predicate.
-- from https://hackage.haskell.org/package/extra-1.7.1/docs/src/Control.Monad.Extra.html#mapMaybeM copied
mapMaybeM :: Monad m => (a -> m (Maybe b)) -> [a] -> m [b]
mapMaybeM op = foldr f (pure [])
  where
    f x xs = do x' <- op x; case x' of { Nothing -> xs; Just x'' -> do { xs' <- xs; pure $ x'' : xs' } }

-- | Lifted version of '<>' which can be used with 'Semigroup's inside 'Applicative's
liftedAppend :: (Applicative f, Semigroup a) => f a -> f a -> f a
liftedAppend = Applicative.liftA2 (<>)
