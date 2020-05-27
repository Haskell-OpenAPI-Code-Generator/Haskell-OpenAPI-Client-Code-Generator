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

-- | Use this security scheme to use basic authentication for a request. Should be used in a 'OpenAPI.Common.Configuration'.
-- 
-- Basic HTTP authentication. Allowed headers-- Authorization: Basic \<api_key> | Authorization: Basic \<base64 hash of \`api_key:\`>
-- 
-- @
-- 'OpenAPI.Configuration.defaultConfiguration'
--   { _securityScheme =
--       'BasicAuthenticationSecurityScheme'
--         { 'basicAuthenticationSecuritySchemeUsername' = "user",
--           'basicAuthenticationSecuritySchemePassword' = "pw"
--         }
--   }
-- @
data BasicAuthenticationSecurityScheme
    = BasicAuthenticationSecurityScheme {basicAuthenticationSecuritySchemeUsername :: Data.Text.Internal.Text,
                                         basicAuthenticationSecuritySchemePassword :: Data.Text.Internal.Text}
    deriving (GHC.Show.Show, GHC.Classes.Ord, GHC.Classes.Eq)

instance OpenAPI.Common.SecurityScheme BasicAuthenticationSecurityScheme
    where authenticateRequest basicAuth = Network.HTTP.Client.Request.applyBasicAuth (OpenAPI.Common.textToByte GHC.Base.$ basicAuthenticationSecuritySchemeUsername basicAuth) (OpenAPI.Common.textToByte GHC.Base.$ basicAuthenticationSecuritySchemePassword basicAuth)

-- | Use this security scheme to use bearer authentication for a request. Should be used in a 'OpenAPI.Common.Configuration'.
-- 
-- Bearer HTTP authentication. Allowed headers-- Authorization: Bearer \<api_key>
-- 
-- @
-- 'OpenAPI.Configuration.defaultConfiguration'
--   { _securityScheme = 'BearerAuthenticationSecurityScheme' "token"
--   }
-- @
data BearerAuthenticationSecurityScheme
    = BearerAuthenticationSecurityScheme Data.Text.Internal.Text
    deriving (GHC.Show.Show, GHC.Classes.Ord, GHC.Classes.Eq)

instance OpenAPI.Common.SecurityScheme BearerAuthenticationSecurityScheme
    where authenticateRequest (BearerAuthenticationSecurityScheme token) = Network.HTTP.Simple.addRequestHeader "Authorization" GHC.Base.$ (OpenAPI.Common.textToByte GHC.Base.$ ("Bearer " GHC.Base.<> token))
