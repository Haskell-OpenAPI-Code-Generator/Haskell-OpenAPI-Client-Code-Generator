{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}

-- | Provides the generation functions for the supported security schemes
module OpenAPI.Generate.SecurityScheme
  ( defineSupportedSecuritySchemes,
  )
where

import qualified Data.Bifunctor as BF
import qualified Data.Maybe as Maybe
import Data.Text (Text)
import Language.Haskell.TH
import Language.Haskell.TH.PprLib hiding ((<>))
import qualified Network.HTTP.Client as HC
import qualified Network.HTTP.Simple as HS
import qualified OpenAPI.Common as OC
import qualified OpenAPI.Generate.Doc as Doc
import qualified OpenAPI.Generate.Monad as OAM
import qualified OpenAPI.Generate.Types as OAT

-- | Defines the security schemes which are configured in the OpenAPI specification
--
-- Generates warnings if unsupported schemes are defined in the specification
defineSupportedSecuritySchemes :: Text -> [(Text, OAT.SecuritySchemeObject)] -> OAM.Generator (Q Doc)
defineSupportedSecuritySchemes moduleName securitySchemes = OAM.nested "securitySchemes" $ do
  let securitySchemeDefinitions = fmap (BF.second $ defineSecurityScheme moduleName) securitySchemes
  mapM_
    ( \(name, _) ->
        OAM.nested name
          $ OAM.logWarning
          $ "The security scheme '" <> name <> "' is not supported (currently only http-basic and http-bearer are supported)."
    )
    $ filter (Maybe.isNothing . snd) securitySchemeDefinitions
  pure $ fmap vcat $ mapM (fmap ($$ text "") . snd) $ Maybe.mapMaybe sequence securitySchemeDefinitions

-- | Defines the security scheme for one 'OAT.SecuritySchemeObject'
defineSecurityScheme :: Text -> OAT.SecuritySchemeObject -> Maybe (Q Doc)
defineSecurityScheme moduleName (OAT.HttpSecuritySchemeObject scheme) =
  let description = Doc.escapeText $ Maybe.fromMaybe "" $ OAT.description (scheme :: OAT.HttpSecurityScheme)
   in case OAT.scheme scheme of
        "basic" -> Just $ basicAuthenticationScheme moduleName description
        "bearer" -> Just $ bearerAuthenticationScheme moduleName description
        _ -> Nothing
defineSecurityScheme _ _ = Nothing

-- | The name used in the instance declaration (referencing 'OC.authenticateRequest').
-- It is necessary because it is not possible to fully qualify the name in the instance declaration.
authenticateRequestName :: Name
authenticateRequestName = mkName "authenticateRequest"

-- | BasicAuthentication scheme with simple username and password
basicAuthenticationScheme :: Text -> Text -> Q Doc
basicAuthenticationScheme moduleName description =
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
              [ varBangType usernameName $ bangType (bang noSourceUnpackedness noSourceStrictness) $ conT ''Text,
                varBangType passwordName $ bangType (bang noSourceUnpackedness noSourceStrictness) $ conT ''Text
              ]
          ]
          [derivClause Nothing [conT ''Show, conT ''Ord, conT ''Eq]]
      instanceDefinition =
        instanceD
          (cxt [])
          (appT (conT ''OC.SecurityScheme) (conT dataName))
          [ funD
              authenticateRequestName
              [ clause
                  [varP paramName]
                  ( normalB
                      [|
                        HC.applyBasicAuth
                          (OC.textToByte $ $(varE usernameName) $(varE paramName))
                          (OC.textToByte $ $(varE passwordName) $(varE paramName))
                        |]
                  )
                  []
              ]
          ]
   in vcat
        <$> sequence
          [ ($$ text "")
              . ( Doc.generateHaddockComment
                    [ "Use this security scheme to use basic authentication for a request. Should be used in a 'OpenAPI.Common.Configuration'.",
                      "",
                      description,
                      "",
                      "@",
                      "'" <> moduleName <> ".Configuration.defaultConfiguration'",
                      "  { configSecurityScheme =",
                      "      'BasicAuthenticationSecurityScheme'",
                      "        { 'basicAuthenticationSecuritySchemeUsername' = \"user\",",
                      "          'basicAuthenticationSecuritySchemePassword' = \"pw\"",
                      "        }",
                      "  }",
                      "@"
                    ]
                    $$
                )
              . ppr <$> dataDefinition,
            ppr <$> instanceDefinition
          ]

-- | BearerAuthentication scheme with a bearer token
bearerAuthenticationScheme :: Text -> Text -> Q Doc
bearerAuthenticationScheme moduleName description =
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
              [bangType (bang noSourceUnpackedness noSourceStrictness) $ conT ''Text]
          ]
          [derivClause Nothing [conT ''Show, conT ''Ord, conT ''Eq]]
      instanceDefinition =
        instanceD
          (cxt [])
          (appT (conT ''OC.SecurityScheme) (conT dataName))
          [ funD
              authenticateRequestName
              [ clause
                  [conP dataName [varP tokenName]]
                  ( normalB
                      [|
                        HS.addRequestHeader "Authorization" $ OC.textToByte $ "Bearer " <> $(varE tokenName)
                        |]
                  )
                  []
              ]
          ]
   in vcat
        <$> sequence
          [ ($$ text "")
              . ( Doc.generateHaddockComment
                    [ "Use this security scheme to use bearer authentication for a request. Should be used in a 'OpenAPI.Common.Configuration'.",
                      "",
                      description,
                      "",
                      "@",
                      "'" <> moduleName <> ".Configuration.defaultConfiguration'",
                      "  { configSecurityScheme = 'BearerAuthenticationSecurityScheme' \"token\"",
                      "  }",
                      "@"
                    ]
                    $$
                )
              . ppr
              <$> dataDefinition,
            ppr <$> instanceDefinition
          ]
