-- | Utility functions for 'Language.Haskell.TH.PprLib.Doc' manipulation
module OpenAPI.Generate.Doc
  ( emptyDoc,
    appendDoc,
    generateHaddockComment,
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
import OpenAPI.Generate.Util

emptyDoc :: Applicative f => f Doc
emptyDoc = pure empty

haddockIntro :: Doc
haddockIntro = text "-- |"

haddockLine :: Doc
haddockLine = text "--"

textToDoc :: Text -> Doc
textToDoc = text . T.unpack

line = ($$) . text

emptyLine = line ""

languageExtension = line . ("{-# LANGUAGE " <>) . (<> " #-}")

importQualified = importUnqualified . ("qualified " <>)

importUnqualified = line . ("import " <>)

moduleDeclaration modulePrefix name = line ("module " <> modulePrefix <> "." <> name <> " where")

appendDoc :: Applicative f => f Doc -> f Doc -> f Doc
appendDoc = Applicative.liftA2 ($$)

-- | Generate a Haddock comment with a title line and a body
--
-- Caution: The body is printed on one line, multiline bodies are not supported.
generateHaddockComment :: Text -> Text -> Doc
generateHaddockComment title body =
  haddockIntro <+> textToDoc title
    $$ haddockLine
    $$ haddockLine <+> textToDoc body

-- | Place two documents side-by-side, aligned at the top line
--
-- If one of the documents is longer than the other, the shorter one is extended with empty lines.
-- The lines of the right document are aligned in the same column, no matter if the left document is shorter or longer
--
-- Example usage:
--
-- > show $ sideBySide (text "a") (text "b" $$ text "c")
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

-- | Add the module header to the generated code
addOperationsModuleHeader :: String -> String -> Doc -> Doc
addOperationsModuleHeader mainModuleName moduleName =
  languageExtension "OverloadedStrings"
    . languageExtension "ExplicitForAll"
    . languageExtension "MultiWayIf"
    . languageExtension "DeriveGeneric"
    . emptyLine
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

addSecuritySchemesModuleHeader :: String -> Doc -> Doc
addSecuritySchemesModuleHeader moduleName =
  languageExtension "OverloadedStrings"
    . emptyLine
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

addConfigurationModuleHeader :: String -> Doc -> Doc
addConfigurationModuleHeader moduleName =
  languageExtension "OverloadedStrings"
    . emptyLine
    . moduleDeclaration moduleName "Configuration"
    . emptyLine
    . importQualified "Data.Text"
    . importQualified (moduleName <> ".Common")
    . emptyLine

createModuleHeaderWithReexports :: String -> [String] -> Doc
createModuleHeaderWithReexports moduleName modulesToExport =
  let exports = vcat $ fmap (text . ("module " <>) . (<> ",")) modulesToExport
      imports = vcat $ fmap (text . ("import " <>)) modulesToExport
   in text ("module " <> moduleName <> " (")
        $$ nest
          2
          ( exports
              $$ text ") where"
          )
        $$ text ""
        $$ imports

addModelModuleHeader :: String -> String -> [String] -> Doc -> Doc
addModelModuleHeader mainModuleName moduleName modelModulesToImport =
  languageExtension "OverloadedStrings"
    . languageExtension "DeriveGeneric"
    . emptyLine
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
