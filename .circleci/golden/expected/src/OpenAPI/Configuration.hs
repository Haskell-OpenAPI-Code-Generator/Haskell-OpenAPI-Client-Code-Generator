{-# LANGUAGE OverloadedStrings #-}

-- | Contains the default configuration
module OpenAPI.Configuration where

import qualified Data.Text
import qualified OpenAPI.Common

-- | The default url specified by the OpenAPI specification
-- 
-- @https://api.stripe.com/@
defaultURL = Data.Text.pack ['h',
                             't',
                             't',
                             'p',
                             's',
                             ':',
                             '/',
                             '/',
                             'a',
                             'p',
                             'i',
                             '.',
                             's',
                             't',
                             'r',
                             'i',
                             'p',
                             'e',
                             '.',
                             'c',
                             'o',
                             'm',
                             '/']
-- | The default configuration containing the 'defaultURL' and no authorization
defaultConfiguration = OpenAPI.Common.Configuration defaultURL OpenAPI.Common.AnonymousSecurityScheme