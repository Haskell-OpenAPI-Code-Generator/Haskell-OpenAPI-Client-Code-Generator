{-# LANGUAGE OverloadedStrings #-}

module OpenAPI.Configuration where

import qualified Data.Text
import qualified OpenAPI.Common

defaultURL = Data.Text.pack "https://api.stripe.com/"
defaultConfiguration = OpenAPI.Common.Configuration defaultURL OpenAPI.Common.AnonymousSecurityScheme