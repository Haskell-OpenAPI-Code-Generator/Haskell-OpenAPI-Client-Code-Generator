{-# LANGUAGE CPP #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE UndecidableInstances #-}

-- | This module serves the purpose of defining common functionality which remains the same across all OpenAPI specifications.
module OpenAPI.Common
  ( doCallWithConfiguration,
    doCallWithConfigurationM,
    doBodyCallWithConfiguration,
    doBodyCallWithConfigurationM,
    runWithConfiguration,
    textToByte,
    byteToText,
    stringifyModel,
    anonymousSecurityScheme,
    jsonObjectToList,
    Configuration (..),
    SecurityScheme,
    MonadHTTP (..),
    JsonByteString (..),
    JsonDateTime (..),
    RequestBodyEncoding (..),
    QueryParameter (..),
    Nullable (..),
    ClientT (..),
    ClientM,
  )
where

import qualified Control.Monad.IO.Class as MIO
import qualified Control.Monad.Reader as MR
import qualified Control.Monad.Trans.Class as MT
import qualified Data.Aeson as Aeson
import qualified Data.Aeson.Encoding as Encoding
import Data.Aeson.Text (encodeToTextBuilder)
import qualified Data.Bifunctor as BF
import qualified Data.ByteString as BS
import qualified Data.ByteString.Lazy as LBS
import qualified Data.Maybe as Maybe
import qualified Data.Scientific as Scientific
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.Encoding as TE
import Data.Text.Encoding.Error (lenientDecode)
import Data.Text.Lazy (toStrict)
import Data.Text.Lazy.Builder (toLazyText)
import qualified Data.Time.LocalTime as Time
import qualified Data.Vector as Vector
import qualified Network.HTTP.Client as HC
import qualified Network.HTTP.Simple as HS
import qualified Network.HTTP.Types as HT

#if MIN_VERSION_aeson(2,0,0)
import qualified Data.Aeson.Key as Key
import qualified Data.Aeson.KeyMap as KeyMap
#else
import qualified Data.HashMap.Strict as HMap
#endif

-- | Abstracts the usage of 'Network.HTTP.Simple.httpBS' away,
--  so that it can be used for testing
class (Monad m) => MonadHTTP m where
  httpBS :: HS.Request -> m (HS.Response BS.ByteString)

-- | This instance is the default instance used for production code
instance MonadHTTP IO where
  httpBS = HS.httpBS

instance (MonadHTTP m) => MonadHTTP (MR.ReaderT r m) where
  httpBS = MT.lift . httpBS

instance (MonadHTTP m) => MonadHTTP (ClientT m) where
  httpBS = MT.lift . httpBS

-- | The monad in which the operations can be run.
-- Contains the 'Configuration' to run the requests with.
--
-- Run it with 'runWithConfiguration'
newtype ClientT m a = ClientT (MR.ReaderT Configuration m a)
  deriving (Functor, Applicative, Monad, MR.MonadReader Configuration)

instance MT.MonadTrans ClientT where
  lift = ClientT . MT.lift

instance (MIO.MonadIO m) => MIO.MonadIO (ClientT m) where
  liftIO = ClientT . MIO.liftIO

-- | Utility type which uses 'IO' as underlying monad
type ClientM a = ClientT IO a

-- | Run a 'ClientT' monad transformer in another monad with a specified configuration
runWithConfiguration :: Configuration -> ClientT m a -> m a
runWithConfiguration c (ClientT r) = MR.runReaderT r c

-- | An operation can and must be configured with data, which may be common
-- for many operations.
--
-- This configuration consists of information about the server URL and the used security scheme.
--
-- In OpenAPI these information can be defined
--
-- * Root level
-- * Path level
-- * Operation level
--
-- To get started, the 'OpenAPI.Configuration.defaultConfiguration' can be used and changed accordingly.
--
-- Note that it is possible that @bearerAuthenticationSecurityScheme@ is not available because it is not a security scheme in the OpenAPI specification.
--
-- > defaultConfiguration
-- >   { configSecurityScheme = bearerAuthenticationSecurityScheme "token" }
data Configuration = Configuration
  { -- | The path of the operation is appended to this URL
    configBaseURL :: Text,
    -- | The 'SecurityScheme' which is applied to the request
    -- This is used to set the @Authentication@ header for example
    configSecurityScheme :: SecurityScheme,
    -- | This flag indicates if an automatically generated @User-Agent@ header
    -- should be added to the request. This allows the server to detect with
    -- which version of the generator the code was generated.
    configIncludeUserAgent :: Bool,
    -- | The application name which will be included in the @User-Agent@ header
    -- if 'configIncludeUserAgent' is set to 'True'
    configApplicationName :: Text
  }

-- | Defines how a request body is encoded
data RequestBodyEncoding
  = -- | Encode the body as JSON
    RequestBodyEncodingJSON
  | -- | Encode the body as form data
    RequestBodyEncodingFormData

-- | Defines a query parameter with the information necessary for serialization
data QueryParameter = QueryParameter
  { queryParamName :: Text,
    queryParamValue :: Maybe Aeson.Value,
    queryParamStyle :: Text,
    queryParamExplode :: Bool
  }
  deriving (Show, Eq)

-- | This type specifies a security scheme which can modify a request according to the scheme (e. g. add an Authorization header)
type SecurityScheme = HS.Request -> HS.Request

-- | Anonymous security scheme which does not alter the request in any way
anonymousSecurityScheme :: SecurityScheme
anonymousSecurityScheme = id

-- | This is the main functionality of this module
--
--   It makes a concrete Call to a Server without a body
doCallWithConfiguration ::
  (MonadHTTP m) =>
  -- | Configuration options like base URL and security scheme
  Configuration ->
  -- | HTTP method (GET, POST, etc.)
  Text ->
  -- | Path to append to the base URL (path parameters should already be replaced)
  Text ->
  -- | Query parameters
  [QueryParameter] ->
  -- | The raw response from the server
  m (HS.Response BS.ByteString)
doCallWithConfiguration config method path queryParams =
  httpBS $ createBaseRequest config method path queryParams

-- | Same as 'doCallWithConfiguration' but run in a 'MR.ReaderT' environment which contains the configuration.
-- This is useful if multiple calls have to be executed with the same configuration.
doCallWithConfigurationM ::
  (MonadHTTP m) =>
  Text ->
  Text ->
  [QueryParameter] ->
  ClientT m (HS.Response BS.ByteString)
doCallWithConfigurationM method path queryParams = do
  config <- MR.ask
  MT.lift $ doCallWithConfiguration config method path queryParams

-- | This is the main functionality of this module
--
--   It makes a concrete Call to a Server with a body
doBodyCallWithConfiguration ::
  (MonadHTTP m, Aeson.ToJSON body) =>
  -- | Configuration options like base URL and security scheme
  Configuration ->
  -- | HTTP method (GET, POST, etc.)
  Text ->
  -- | Path to append to the base URL (path parameters should already be replaced)
  Text ->
  -- | Query parameters
  [QueryParameter] ->
  -- | Request body
  Maybe body ->
  -- | JSON or form data deepobjects
  RequestBodyEncoding ->
  -- | The raw response from the server
  m (HS.Response BS.ByteString)
doBodyCallWithConfiguration config method path queryParams Nothing _ = doCallWithConfiguration config method path queryParams
doBodyCallWithConfiguration config method path queryParams (Just body) RequestBodyEncodingJSON =
  httpBS $ HS.setRequestMethod (textToByte method) $ HS.setRequestBodyJSON body baseRequest
  where
    baseRequest = createBaseRequest config method path queryParams
doBodyCallWithConfiguration config method path queryParams (Just body) RequestBodyEncodingFormData =
  httpBS $ HS.setRequestMethod (textToByte method) $ HS.setRequestBodyURLEncoded byteStringData baseRequest
  where
    baseRequest = createBaseRequest config method path queryParams
    byteStringData = createFormData body

-- | Same as 'doBodyCallWithConfiguration' but run in a 'MR.ReaderT' environment which contains the configuration.
-- This is useful if multiple calls have to be executed with the same configuration.
doBodyCallWithConfigurationM ::
  (MonadHTTP m, Aeson.ToJSON body) =>
  Text ->
  Text ->
  [QueryParameter] ->
  Maybe body ->
  RequestBodyEncoding ->
  ClientT m (HS.Response BS.ByteString)
doBodyCallWithConfigurationM method path queryParams body encoding = do
  config <- MR.ask
  MT.lift $ doBodyCallWithConfiguration config method path queryParams body encoding

-- | Creates a Base Request
createBaseRequest ::
  -- | Configuration options like base URL and security scheme
  Configuration ->
  -- | HTTP method (GET, POST, etc.)
  Text ->
  -- | The path for which the placeholders have already been replaced
  Text ->
  -- | Query Parameters
  [QueryParameter] ->
  -- | The Response from the server
  HS.Request
createBaseRequest config method path queryParams =
  configSecurityScheme config $
    addUserAgent $
      HS.setRequestMethod (textToByte method) $
        HS.setRequestQueryString query $
          HS.setRequestPath
            (textToByte $ basePathModifier <> path)
            baseRequest
  where
    baseRequest = parseURL $ configBaseURL config
    basePath = byteToText $ HC.path baseRequest
    basePathModifier =
      if basePath == "/" && T.isPrefixOf "/" path
        then ""
        else basePath
    -- filters all maybe
    query = BF.second pure <$> serializeQueryParams queryParams
    userAgent = configApplicationName config <> " openapi3-code-generator/VERSION_TO_REPLACE (https://github.com/Haskell-OpenAPI-Code-Generator/Haskell-OpenAPI-Client-Code-Generator)"
    addUserAgent =
      if configIncludeUserAgent config
        then HS.addRequestHeader HT.hUserAgent $ textToByte userAgent
        else id

serializeQueryParams :: [QueryParameter] -> [(BS.ByteString, BS.ByteString)]
serializeQueryParams = (>>= serializeQueryParam)

serializeQueryParam :: QueryParameter -> [(BS.ByteString, BS.ByteString)]
serializeQueryParam QueryParameter {..} =
  let concatValues :: BS.ByteString -> [(Maybe Text, BS.ByteString)] -> [(Text, BS.ByteString)]
      concatValues joinWith =
        if queryParamExplode
          then fmap (BF.first $ Maybe.fromMaybe queryParamName)
          else
            pure
              . (queryParamName,)
              . BS.intercalate joinWith
              . fmap
                ( \case
                    (Nothing, value) -> value
                    (Just name, value) -> textToByte name <> joinWith <> value
                )
   in BF.first textToByte <$> case queryParamValue of
        Nothing -> []
        Just value ->
          ( case queryParamStyle of
              "form" -> concatValues ","
              "spaceDelimited" -> concatValues " "
              "pipeDelimited" -> concatValues "|"
              "deepObject" -> const $ BF.second textToByte <$> jsonToFormDataPrefixed queryParamName value
              _ -> const []
          )
            $ jsonToFormDataFlat Nothing value

encodeStrict :: (Aeson.ToJSON a) => a -> BS.ByteString
encodeStrict = LBS.toStrict . Aeson.encode

jsonToFormDataFlat :: Maybe Text -> Aeson.Value -> [(Maybe Text, BS.ByteString)]
jsonToFormDataFlat _ Aeson.Null = []
jsonToFormDataFlat name (Aeson.Number a) = [(name, encodeStrict a)]
jsonToFormDataFlat name (Aeson.String a) = [(name, textToByte a)]
jsonToFormDataFlat name (Aeson.Bool a) = [(name, encodeStrict a)]
jsonToFormDataFlat _ (Aeson.Object object) = jsonObjectToList object >>= uncurry jsonToFormDataFlat . BF.first Just
jsonToFormDataFlat name (Aeson.Array vector) = Vector.toList vector >>= jsonToFormDataFlat name

-- | creates form data bytestring array
createFormData :: (Aeson.ToJSON a) => a -> [(BS.ByteString, BS.ByteString)]
createFormData body =
  let formData = jsonToFormData $ Aeson.toJSON body
   in fmap (BF.bimap textToByte textToByte) formData

-- | Convert a 'BS.ByteString' to 'Text'
byteToText :: BS.ByteString -> Text
byteToText = TE.decodeUtf8With lenientDecode

-- | Convert 'Text' a to 'BS.ByteString'
textToByte :: Text -> BS.ByteString
textToByte = TE.encodeUtf8

parseURL :: Text -> HS.Request
parseURL url =
  Maybe.fromMaybe HS.defaultRequest $
    HS.parseRequest $
      T.unpack url

jsonToFormData :: Aeson.Value -> [(Text, Text)]
jsonToFormData = jsonToFormDataPrefixed ""

jsonToFormDataPrefixed :: Text -> Aeson.Value -> [(Text, Text)]
jsonToFormDataPrefixed prefix (Aeson.Number a) = case Scientific.toBoundedInteger a :: Maybe Int of
  Just myInt -> [(prefix, T.pack $ show myInt)]
  Nothing -> [(prefix, T.pack $ show a)]
jsonToFormDataPrefixed prefix (Aeson.Bool True) = [(prefix, "true")]
jsonToFormDataPrefixed prefix (Aeson.Bool False) = [(prefix, "false")]
jsonToFormDataPrefixed _ Aeson.Null = []
jsonToFormDataPrefixed prefix (Aeson.String a) = [(prefix, a)]
jsonToFormDataPrefixed "" (Aeson.Object object) =
  jsonObjectToList object >>= uncurry jsonToFormDataPrefixed
jsonToFormDataPrefixed prefix (Aeson.Object object) =
  jsonObjectToList object >>= (\(x, y) -> jsonToFormDataPrefixed (prefix <> "[" <> x <> "]") y)
jsonToFormDataPrefixed prefix (Aeson.Array vector) =
  Vector.toList vector >>= jsonToFormDataPrefixed (prefix <> "[]")

-- | This function makes the code generation for URL parameters easier as it allows to stringify a value
--
-- The 'Show' class is not sufficient as strings should not be stringified with quotes.
stringifyModel :: (Aeson.ToJSON a) => a -> Text
stringifyModel x = case Aeson.toJSON x of
  Aeson.String s -> s
  v -> toStrict $ toLazyText $ encodeToTextBuilder v

-- | Wraps a 'BS.ByteString' to implement 'Aeson.ToJSON' and 'Aeson.FromJSON'
newtype JsonByteString = JsonByteString BS.ByteString
  deriving (Show, Eq, Ord)

instance Aeson.ToJSON JsonByteString where
  toJSON (JsonByteString s) = Aeson.toJSON $ byteToText s

instance Aeson.FromJSON JsonByteString where
  parseJSON (Aeson.String s) = pure $ JsonByteString $ textToByte s
  parseJSON _ = fail "Value cannot be converted to a 'JsonByteString'"

-- | Wraps a 'Time.ZonedTime' to implement 'Aeson.ToJSON' and 'Aeson.FromJSON'
newtype JsonDateTime = JsonDateTime Time.ZonedTime
  deriving (Show)

instance Eq JsonDateTime where
  (JsonDateTime d1) == (JsonDateTime d2) = Time.zonedTimeToUTC d1 == Time.zonedTimeToUTC d2

instance Ord JsonDateTime where
  (JsonDateTime d1) <= (JsonDateTime d2) = Time.zonedTimeToUTC d1 <= Time.zonedTimeToUTC d2

instance Aeson.ToJSON JsonDateTime where
  toJSON (JsonDateTime d) = Aeson.toJSON d

instance Aeson.FromJSON JsonDateTime where
  parseJSON o = JsonDateTime <$> Aeson.parseJSON o

data Nullable a = NonNull a | Null
  deriving (Show, Eq)

instance (Aeson.ToJSON a) => Aeson.ToJSON (Nullable a) where
  toJSON Null = Aeson.Null
  toJSON (NonNull x) = Aeson.toJSON x

  toEncoding Null = Encoding.null_
  toEncoding (NonNull x) = Aeson.toEncoding x

instance (Aeson.FromJSON a) => Aeson.FromJSON (Nullable a) where
  parseJSON Aeson.Null = pure Null
  parseJSON x = NonNull <$> Aeson.parseJSON x

#if MIN_VERSION_aeson(2,0,0)
jsonObjectToList :: KeyMap.KeyMap v -> [(Text, v)]
jsonObjectToList = fmap (BF.first Key.toText) . KeyMap.toList
#else
jsonObjectToList :: HMap.HashMap Text v -> [(Text, v)]
jsonObjectToList = HMap.toList
#endif
