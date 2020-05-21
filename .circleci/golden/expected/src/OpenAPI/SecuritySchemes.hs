{-# LANGUAGE OverloadedStrings #-}

-- | Contains all supported security schemes defined in the specification
module OpenAPI.SecuritySchemes where

import qualified Data.Text.Internal
import qualified GHC.Base
import qualified GHC.Classes
import qualified GHC.Show
import qualified Network.HTTP.Client as Network.HTTP.Client.Request
import qualified Network.HTTP.Simple
import qualified OpenAPI.Common
