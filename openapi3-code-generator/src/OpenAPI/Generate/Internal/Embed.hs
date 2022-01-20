module OpenAPI.Generate.Internal.Embed where

import Language.Haskell.TH
import Language.Haskell.TH.Syntax

embedFile :: FilePath -> Q Exp
embedFile fp = LitE . StringL <$> (qAddDependentFile fp >> runIO (readFile fp))
