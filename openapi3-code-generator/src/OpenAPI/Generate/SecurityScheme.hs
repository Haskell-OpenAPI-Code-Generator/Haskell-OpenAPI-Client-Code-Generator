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
import Data.Text (Text, unpack)
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
        OAM.nested name $
          OAM.logWarning $
            "The security scheme '" <> name <> "' is not supported (currently only http-basic and http-bearer are supported)."
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
defineSecurityScheme moduleName (OAT.ApiKeySecuritySchemeObject scheme) =
  let description = Doc.escapeText $ Maybe.fromMaybe "" $ OAT.description (scheme :: OAT.ApiKeySecurityScheme)
      name = OAT.name (scheme :: OAT.ApiKeySecurityScheme)
   in case OAT.in' (scheme :: OAT.ApiKeySecurityScheme) of
        OAT.HeaderApiKeySecuritySchemeLocation -> Just $ apiKeyInHeaderAuthenticationScheme name moduleName description
        _ -> Nothing
defineSecurityScheme _ _ = Nothing

-- | BasicAuthentication scheme with simple username and password
basicAuthenticationScheme :: Text -> Text -> Q Doc
basicAuthenticationScheme moduleName description =
  let dataName = mkName "BasicAuthenticationData"
      usernameName = mkName "basicAuthenticationDataUsername"
      passwordName = mkName "basicAuthenticationDataPassword"
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
      fnName = mkName "basicAuthenticationSecurityScheme"
      functionType = sigD fnName [t|$(varT dataName) -> OC.SecurityScheme|]
      functionBody =
        [d|
          $(varP fnName) = \basicAuth ->
            HC.applyBasicAuth
              (OC.textToByte $ $(varE usernameName) basicAuth)
              (OC.textToByte $ $(varE passwordName) basicAuth)
          |]
   in vcat
        <$> sequence
          [ ($$ text "")
              . ( Doc.generateHaddockComment
                    [ "Used to pass the authentication information for BasicAuthentication to 'basicAuthenticationSecurityScheme'."
                    ]
                    $$
                )
              . ppr
              <$> dataDefinition,
            ( Doc.generateHaddockComment
                [ "Use this security scheme to use basic authentication for a request. Should be used in a 'OpenAPI.Common.Configuration'.",
                  "",
                  description,
                  "",
                  "@",
                  "'" <> moduleName <> ".Configuration.defaultConfiguration'",
                  "  { configSecurityScheme =",
                  "      'basicAuthenticationSecurityScheme' 'BasicAuthenticationData'",
                  "        { 'basicAuthenticationDataUsername' = \"user\",",
                  "          'basicAuthenticationDataPassword' = \"pw\"",
                  "        }",
                  "  }",
                  "@"
                ]
                $$
            )
              . ppr
              <$> functionType,
            ppr <$> functionBody
          ]

-- | BearerAuthentication scheme with a bearer token
bearerAuthenticationScheme :: Text -> Text -> Q Doc
bearerAuthenticationScheme moduleName description =
  let fnName = mkName "bearerAuthenticationSecurityScheme"
      functionType = sigD fnName [t|Text -> OC.SecurityScheme|]
      functionBody = [d|$(varP fnName) = \token -> HS.addRequestHeader "Authorization" $ OC.textToByte $ "Bearer " <> token|]
   in vcat
        <$> sequence
          [ ( Doc.generateHaddockComment
                [ "Use this security scheme to use bearer authentication for a request. Should be used in a 'OpenAPI.Common.Configuration'.",
                  "",
                  description,
                  "",
                  "@",
                  "'" <> moduleName <> ".Configuration.defaultConfiguration'",
                  "  { configSecurityScheme = 'bearerAuthenticationSecurityScheme' \"token\"",
                  "  }",
                  "@"
                ]
                $$
            )
              . ppr
              <$> functionType,
            ppr <$> functionBody
          ]

-- | ApiKeyAuthentication scheme with a bearer token
apiKeyInHeaderAuthenticationScheme :: Text -> Text -> Text -> Q Doc
apiKeyInHeaderAuthenticationScheme headerName moduleName description =
  let fnName = mkName "apiKeyInHeaderAuthenticationSecurityScheme"
      functionType = sigD fnName [t|Text -> OC.SecurityScheme|]
      headerName' = stringE $ Data.Text.unpack headerName
      functionBody = [d|$(varP fnName) = HS.addRequestHeader $(headerName') . OC.textToByte|]
   in vcat
        <$> sequence
          [ ( Doc.generateHaddockComment
                [ "Use this security scheme to use token in HTTP header for authentication. Should be used in a 'OpenAPI.Common.Configuration'.",
                  "",
                  description,
                  "",
                  "@",
                  "'" <> moduleName <> ".Configuration.defaultConfiguration'",
                  "  { configSecurityScheme = 'apiKeyInHeaderAuthenticationSecurityScheme' \"token\"",
                  "  }",
                  "@"
                ]
                $$
            )
              . ppr
              <$> functionType,
            ppr <$> functionBody
          ]
