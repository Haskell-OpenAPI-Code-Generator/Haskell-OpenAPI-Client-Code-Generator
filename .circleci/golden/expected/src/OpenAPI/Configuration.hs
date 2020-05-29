{-# LANGUAGE OverloadedStrings #-}

-- | Contains the default configuration
module OpenAPI.Configuration where

import qualified Data.Text
import qualified OpenAPI.Common

-- | The default url specified by the OpenAPI specification
-- 
-- @https://api.stripe.com/@
defaultURL = Data.Text.pack "https://api.stripe.com/"
-- | The default configuration containing the 'defaultURL' and no authorization
defaultConfiguration = OpenAPI.Common.Configuration defaultURL OpenAPI.Common.AnonymousSecurityScheme