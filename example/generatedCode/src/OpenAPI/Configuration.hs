{-# LANGUAGE OverloadedStrings #-}

-- | Contains the default configuration
module OpenAPI.Configuration where

import qualified Data.Text
import qualified OpenAPI.Common

-- | The default url specified by the OpenAPI specification
--
-- @https://petstore.swagger.io/v2@
defaultURL = Data.Text.pack "https://petstore.swagger.io/v2"

-- | The default configuration containing the 'defaultURL' and no authorization
defaultConfiguration = OpenAPI.Common.Configuration defaultURL OpenAPI.Common.AnonymousSecurityScheme
