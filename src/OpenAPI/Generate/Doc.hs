{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}

-- | Utility functions for 'Language.Haskell.TH.PprLib.Doc' manipulation
module OpenAPI.Generate.Doc
  ( emptyDoc,
    appendDoc,
    generateHaddockComment,
    escapeText,
    breakOnTokens,
    breakOnTokensWithReplacement,
    sideComments,
    zipCodeAndComments,
    sideBySide,
    addOperationsModuleHeader,
    addSecuritySchemesModuleHeader,
    addConfigurationModuleHeader,
    createModuleHeaderWithReexports,
    addModelModuleHeader,
  )
where

import qualified Control.Applicative as Applicative
import Data.Text (Text)
import qualified Data.Text as T
import Language.Haskell.TH.PprLib hiding ((<>))
import OpenAPI.Generate.Internal.Util

-- | Empty document inside an 'Applicative' (typically 'Language.Haskell.TH.Q')
emptyDoc :: Applicative f => f Doc
emptyDoc = pure empty

haddockIntro :: Doc
haddockIntro = text "-- |"

haddockLine :: Doc
haddockLine = text "--"

textToDoc :: Text -> Doc
textToDoc = text . T.unpack

line :: String -> Doc -> Doc
line = ($$) . text

emptyLine :: Doc -> Doc
emptyLine = line ""

languageExtension :: String -> Doc -> Doc
languageExtension = line . ("{-# LANGUAGE " <>) . (<> " #-}")

importQualified :: String -> Doc -> Doc
importQualified = importUnqualified . ("qualified " <>)

importUnqualified :: String -> Doc -> Doc
importUnqualified = line . ("import " <>)

moduleDescription :: String -> Doc -> Doc
moduleDescription = line . ("-- | " <>)

moduleDeclaration :: String -> String -> Doc -> Doc
moduleDeclaration modulePrefix name = line ("module " <> modulePrefix <> "." <> name <> " where")

-- | Append a 'Doc' to another inside an 'Applicative' (typically 'Language.Haskell.TH.Q')
appendDoc :: Applicative f => f Doc -> f Doc -> f Doc
appendDoc = Applicative.liftA2 ($$)

-- | Generate a Haddock comment with multiple lines
generateHaddockComment :: [Text] -> Doc
generateHaddockComment =
  generateHaddockCommentWithoutNewlines
    . ( >>=
          ( \case
              [] -> [""]
              x -> x
          )
            . T.lines
      )

generateHaddockCommentWithoutNewlines :: [Text] -> Doc
generateHaddockCommentWithoutNewlines [] = empty
generateHaddockCommentWithoutNewlines [x] = haddockIntro <+> textToDoc x
generateHaddockCommentWithoutNewlines xs =
  generateHaddockCommentWithoutNewlines (init xs)
    $$ haddockLine <+> textToDoc (last xs)

-- | Escape text for use in Haddock comment
escapeText :: Text -> Text
escapeText =
  T.replace "'" "\\'"
    . T.replace "\"" "\\\""
    . T.replace "`" "\\`"
    . T.replace "@" "\\@"
    . T.replace "$" "\\$"
    . T.replace "#" "\\#"
    . T.replace "<" "\\<"
    . T.replace "/" "\\/"
    . T.replace "\\" "\\\\"

-- | Add line breaks to a 'Doc' at all occurrences of the passed tokens (removes all other line breaks).
breakOnTokens :: [Text] -> Doc -> Doc
breakOnTokens = breakOnTokensWithReplacement ("\n  " <>)

-- | Add line breaks to a 'Doc' at all occurrences of the passed tokens (removes all other line breaks).
-- The replacement function is used to generate the text replacing the tokens.
breakOnTokensWithReplacement :: (Text -> Text) -> [Text] -> Doc -> Doc
breakOnTokensWithReplacement replaceFn tokens =
  let addLineBreaks = foldr (\token f -> T.replace token (replaceFn token) . f) id tokens
   in text . T.unpack . addLineBreaks . T.replace "\n" "" . removeDuplicateSpaces . T.pack . show

removeDuplicateSpaces :: Text -> Text
removeDuplicateSpaces t =
  let t' = T.replace "  " " " t
   in if t == t' then t' else removeDuplicateSpaces t'

-- | Convert a list of lines to side comments
sideComments :: [Text] -> Doc
sideComments = vcat . fmap (text . T.unpack . T.replace "\n" " " . ("-- ^ " <>))

-- | Intertwine code lines with comment lines
--
-- The code lines should have one more line (the first line is not commented)
zipCodeAndComments :: [Text] -> [Text] -> Doc
zipCodeAndComments [] _ = empty
zipCodeAndComments [x] _ = textToDoc x
zipCodeAndComments (x : xs) [] = textToDoc x $$ zipCodeAndComments xs []
zipCodeAndComments (x : xs) (y : ys) = textToDoc x $$ nest 2 (generateHaddockComment [y]) $$ zipCodeAndComments xs ys

-- | Place two documents side-by-side, aligned at the top line
--
-- If one of the documents is longer than the other, the shorter one is extended with empty lines.
-- The lines of the right document are aligned in the same column, no matter if the left document is shorter or longer
--
-- Example usage:
--
-- >>> show $ sideBySide (text "a") (text "b" $$ text "c")
-- a b
--   c
sideBySide :: Doc -> Doc -> Doc
sideBySide leftDoc rightDoc =
  let splitDoc = splitOn '\n' . show
      leftDocLines = splitDoc leftDoc
      leftDoc' = map text leftDocLines
      maxLength = foldr max 0 (fmap length leftDocLines) + 1
      rightDoc' = map (nest maxLength . text) . splitDoc $ rightDoc
      isLeftLonger = length leftDoc' > length rightDoc'
      isRightLonger = length leftDoc' < length rightDoc'
   in foldl ($$) empty $
        zipWith
          ($$)
          (if isRightLonger then leftDoc' <> repeat empty else leftDoc')
          (if isLeftLonger then rightDoc' <> repeat empty else rightDoc')

-- | Add the module header to a module of an operation
addOperationsModuleHeader :: String -> String -> String -> Doc -> Doc
addOperationsModuleHeader mainModuleName moduleName operationId =
  languageExtension "OverloadedStrings"
    . languageExtension "ExplicitForAll"
    . languageExtension "MultiWayIf"
    . languageExtension "DeriveGeneric"
    . emptyLine
    . moduleDescription ("Contains the different functions to run the operation " <> operationId)
    . moduleDeclaration (mainModuleName <> ".Operations") moduleName
    . emptyLine
    . importQualified "Prelude as GHC.Integer.Type"
    . importQualified "Prelude as GHC.Maybe"
    . importQualified "Control.Monad.Trans.Reader"
    . importQualified "Data.Aeson"
    . importQualified "Data.Aeson as Data.Aeson.Types"
    . importQualified "Data.Aeson as Data.Aeson.Types.FromJSON"
    . importQualified "Data.Aeson as Data.Aeson.Types.ToJSON"
    . importQualified "Data.Aeson as Data.Aeson.Types.Internal"
    . importQualified "Data.ByteString.Char8"
    . importQualified "Data.ByteString.Char8 as Data.ByteString.Internal"
    . importQualified "Data.Either"
    . importQualified "Data.Functor"
    . importQualified "Data.Scientific"
    . importQualified "Data.Text"
    . importQualified "Data.Text.Internal"
    . importQualified "Data.Time.Calendar as Data.Time.Calendar.Days"
    . importQualified "Data.Time.LocalTime as Data.Time.LocalTime.Internal.ZonedTime"
    . importQualified "GHC.Base"
    . importQualified "GHC.Classes"
    . importQualified "GHC.Generics"
    . importQualified "GHC.Int"
    . importQualified "GHC.Show"
    . importQualified "GHC.Types"
    . importQualified "Network.HTTP.Client"
    . importQualified "Network.HTTP.Client as Network.HTTP.Client.Request"
    . importQualified "Network.HTTP.Client as Network.HTTP.Client.Types"
    . importQualified "Network.HTTP.Simple"
    . importQualified "Network.HTTP.Types"
    . importQualified "Network.HTTP.Types as Network.HTTP.Types.Status"
    . importQualified "Network.HTTP.Types as Network.HTTP.Types.URI"
    . importQualified (mainModuleName <> ".Common")
    . importUnqualified (mainModuleName <> ".Types")
    . emptyLine

-- | Add the module header to a module of a model
addModelModuleHeader :: String -> String -> [String] -> String -> Doc -> Doc
addModelModuleHeader mainModuleName moduleName modelModulesToImport description =
  languageExtension "OverloadedStrings"
    . languageExtension "DeriveGeneric"
    . emptyLine
    . moduleDescription description
    . moduleDeclaration mainModuleName moduleName
    . emptyLine
    . importQualified "Prelude as GHC.Integer.Type"
    . importQualified "Prelude as GHC.Maybe"
    . importQualified "Data.Aeson"
    . importQualified "Data.Aeson as Data.Aeson.Types"
    . importQualified "Data.Aeson as Data.Aeson.Types.FromJSON"
    . importQualified "Data.Aeson as Data.Aeson.Types.ToJSON"
    . importQualified "Data.Aeson as Data.Aeson.Types.Internal"
    . importQualified "Data.ByteString.Char8"
    . importQualified "Data.ByteString.Char8 as Data.ByteString.Internal"
    . importQualified "Data.Functor"
    . importQualified "Data.Scientific"
    . importQualified "Data.Text"
    . importQualified "Data.Text.Internal"
    . importQualified "Data.Time.Calendar as Data.Time.Calendar.Days"
    . importQualified "Data.Time.LocalTime as Data.Time.LocalTime.Internal.ZonedTime"
    . importQualified "GHC.Base"
    . importQualified "GHC.Classes"
    . importQualified "GHC.Generics"
    . importQualified "GHC.Int"
    . importQualified "GHC.Show"
    . importQualified "GHC.Types"
    . importQualified (mainModuleName <> ".Common")
    . (vcat (fmap (text . ("import " <>) . ((mainModuleName <> ".") <>)) modelModulesToImport) $$)
    . emptyLine

-- | Add the module header to the security scheme module
addSecuritySchemesModuleHeader :: String -> Doc -> Doc
addSecuritySchemesModuleHeader moduleName =
  languageExtension "OverloadedStrings"
    . emptyLine
    . moduleDescription "Contains all supported security schemes defined in the specification"
    . moduleDeclaration moduleName "SecuritySchemes"
    . emptyLine
    . importQualified "Data.Text.Internal"
    . importQualified "GHC.Base"
    . importQualified "GHC.Classes"
    . importQualified "GHC.Show"
    . importQualified "Network.HTTP.Client as Network.HTTP.Client.Request"
    . importQualified "Network.HTTP.Simple"
    . importQualified (moduleName <> ".Common")
    . emptyLine

-- | Add the module header to the configuration module
addConfigurationModuleHeader :: String -> Doc -> Doc
addConfigurationModuleHeader moduleName =
  languageExtension "OverloadedStrings"
    . emptyLine
    . moduleDescription "Contains the default configuration"
    . moduleDeclaration moduleName "Configuration"
    . emptyLine
    . importQualified "Data.Text"
    . importQualified (moduleName <> ".Common")
    . emptyLine

-- | Create a 'Doc' containing a module which imports other modules and re-exports them
createModuleHeaderWithReexports :: String -> [String] -> String -> Doc
createModuleHeaderWithReexports moduleName modulesToExport description =
  let exports = vcat $ fmap (text . ("module " <>) . (<> ",")) modulesToExport
      imports = vcat $ fmap (text . ("import " <>)) modulesToExport
   in moduleDescription description $
        text ("module " <> moduleName <> " (")
          $$ nest
            2
            ( exports
                $$ text ") where"
            )
          $$ text ""
          $$ imports
