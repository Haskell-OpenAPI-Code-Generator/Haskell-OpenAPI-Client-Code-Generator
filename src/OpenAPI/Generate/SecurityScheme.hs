{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}

-- | Provides the generation functions for the supported security schemes
module OpenAPI.Generate.SecurityScheme
  ( defineSupportedSecuritySchemes,
  )
where

import Data.Bifunctor
import qualified Data.ByteString.Char8 as B8
import qualified Data.Map as Map
import Data.Maybe
import Data.Text (Text)
import qualified Data.Text as T
import Language.Haskell.TH
import Language.Haskell.TH.PprLib hiding ((<>))
import qualified Network.HTTP.Client as HC
import qualified Network.HTTP.Simple as HS
import qualified OpenAPI.Common as OAC
import qualified OpenAPI.Generate.Monad as OAM
import qualified OpenAPI.Generate.Types as OAT

-- | Defines the security schemes which are configured in the OpenAPI specification
--
-- Generates warnings if unsupported schemes are defined in the specification
defineSupportedSecuritySchemes :: [(Text, OAT.SecuritySchemeObject)] -> OAM.Generator (Q Doc)
defineSupportedSecuritySchemes securitySchemes = OAM.nested "securitySchemes" $ do
  let securitySchemeDefinitions = fmap (second defineSecurityScheme) securitySchemes
  mapM_
    ( \(name, _) ->
        OAM.nested name
          $ OAM.logWarning
          $ "The security scheme '" <> name <> "' is not supported (currently only http-basic and http-bearer are supported)."
    )
    $ filter (isNothing . snd) securitySchemeDefinitions
  pure $ fmap vcat $ mapM (fmap ($$ text "") . snd) $ mapMaybe sequence securitySchemeDefinitions

-- | Defines the security scheme for one 'OAT.SecuritySchemeObject'
defineSecurityScheme :: OAT.SecuritySchemeObject -> Maybe (Q Doc)
defineSecurityScheme (OAT.HttpSecuritySchemeObject scheme) = case OAT.scheme scheme of
  "basic" -> Just basicAuthenticationScheme
  "bearer" -> Just bearerAuthenticationScheme
  _ -> Nothing
defineSecurityScheme _ = Nothing

-- | The name used in the instance declaration (referencing 'OAC.authenticateRequest').
-- It is necessary because it is not possible to fully qualify the name in the instance declaration.
authenticateRequestName :: Name
authenticateRequestName = mkName "authenticateRequest"

-- | BasicAuthentication scheme with simple username and password
basicAuthenticationScheme :: Q Doc
basicAuthenticationScheme =
  let dataName = mkName "BasicAuthenticationSecurityScheme"
      usernameName = mkName "basicAuthenticationSecuritySchemeUsername"
      passwordName = mkName "basicAuthenticationSecuritySchemePassword"
      paramName = mkName "basicAuth"
      dataDefinition =
        dataD
          (cxt [])
          dataName
          []
          Nothing
          [ recC
              dataName
              [ varBangType usernameName $ bangType (bang noSourceUnpackedness noSourceStrictness) $ conT ''T.Text,
                varBangType passwordName $ bangType (bang noSourceUnpackedness noSourceStrictness) $ conT ''T.Text
              ]
          ]
          [derivClause Nothing [conT ''Show, conT ''Ord, conT ''Eq]]
      instanceDefinition =
        instanceD
          (cxt [])
          (appT (conT ''OAC.SecurityScheme) (conT dataName))
          [ funD
              authenticateRequestName
              [ clause
                  [varP paramName]
                  ( normalB
                      [|
                        HC.applyBasicAuth
                          (OAC.textToByte $ $(varE usernameName) $(varE paramName))
                          (OAC.textToByte $ $(varE passwordName) $(varE paramName))
                        |]
                  )
                  []
              ]
          ]
   in vcat <$> sequence [($$ text "") . ppr <$> dataDefinition, ppr <$> instanceDefinition]

-- | BearerAuthentication scheme with a bearer token
bearerAuthenticationScheme :: Q Doc
bearerAuthenticationScheme =
  let dataName = mkName "BearerAuthenticationSecurityScheme"
      tokenName = mkName "token"
      dataDefinition =
        dataD
          (cxt [])
          dataName
          []
          Nothing
          [ normalC
              dataName
              [bangType (bang noSourceUnpackedness noSourceStrictness) $ conT ''T.Text]
          ]
          [derivClause Nothing [conT ''Show, conT ''Ord, conT ''Eq]]
      instanceDefinition =
        instanceD
          (cxt [])
          (appT (conT ''OAC.SecurityScheme) (conT dataName))
          [ funD
              authenticateRequestName
              [ clause
                  [conP dataName [varP tokenName]]
                  ( normalB
                      [|
                        HS.addRequestHeader "Authorization" $ OAC.textToByte $ "Bearer " <> $(varE tokenName)
                        |]
                  )
                  []
              ]
          ]
   in vcat <$> sequence [($$ text "") . ppr <$> dataDefinition, ppr <$> instanceDefinition]
