{-# LANGUAGE OverloadedStrings #-}

module OpenAPI.Generate.Internal.Embed where

import Data.Maybe
import qualified Data.Text as T
import Language.Haskell.TH
import Language.Haskell.TH.Syntax

embedFile :: FilePath -> Q Exp
embedFile fp = LitE . StringL <$> (qAddDependentFile fp >> runIO (readFile fp))

embedPackageVersionNumber :: FilePath -> Q Exp
embedPackageVersionNumber fp = do
  fileContents <- qAddDependentFile fp >> runIO (readFile fp)
  let versionNumberLines = mapMaybe (fmap T.strip . T.stripPrefix "version:") $ T.lines (T.pack fileContents)
      version = case versionNumberLines of
        v : _ -> T.unpack v
        [] -> fail $ "Could not extract version number from file: " <> fp
  litE $ StringL version
