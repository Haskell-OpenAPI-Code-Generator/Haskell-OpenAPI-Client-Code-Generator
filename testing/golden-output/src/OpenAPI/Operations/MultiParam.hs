-- CHANGE WITH CAUTION: This is a generated code file generated by https://github.com/Haskell-OpenAPI-Code-Generator/Haskell-OpenAPI-Client-Code-Generator.

{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ExplicitForAll #-}
{-# LANGUAGE MultiWayIf #-}

-- | Contains the different functions to run the operation multiParam
module OpenAPI.Operations.MultiParam where

import qualified Prelude as GHC.Integer.Type
import qualified Prelude as GHC.Maybe
import qualified Control.Monad.Fail
import qualified Control.Monad.Trans.Reader
import qualified Data.Aeson
import qualified Data.Aeson as Data.Aeson.Decoding
import qualified Data.Aeson as Data.Aeson.Encoding.Internal
import qualified Data.Aeson as Data.Aeson.Types
import qualified Data.Aeson as Data.Aeson.Types.FromJSON
import qualified Data.Aeson as Data.Aeson.Types.Internal
import qualified Data.Aeson as Data.Aeson.Types.ToJSON
import qualified Data.ByteString
import qualified Data.ByteString as Data.ByteString.Internal
import qualified Data.ByteString as Data.ByteString.Internal.Type
import qualified Data.Either
import qualified Data.Foldable
import qualified Data.Functor
import qualified Data.Maybe
import qualified Data.Scientific
import qualified Data.Text
import qualified Data.Text as Data.Text.Internal
import qualified Data.Time.Calendar as Data.Time.Calendar.Days
import qualified Data.Time.LocalTime as Data.Time.LocalTime.Internal.ZonedTime
import qualified Data.Vector
import qualified GHC.Base
import qualified GHC.Classes
import qualified GHC.Int
import qualified GHC.Show
import qualified GHC.Types
import qualified Network.HTTP.Client
import qualified Network.HTTP.Client as Network.HTTP.Client.Request
import qualified Network.HTTP.Client as Network.HTTP.Client.Types
import qualified Network.HTTP.Simple
import qualified Network.HTTP.Types
import qualified Network.HTTP.Types as Network.HTTP.Types.Status
import qualified Network.HTTP.Types as Network.HTTP.Types.URI
import qualified OpenAPI.Common
import OpenAPI.Types

-- | > GET /pet/multiparam/{status}
-- 
-- Operation with multiple parameters
multiParam :: forall m . OpenAPI.Common.MonadHTTP m => MultiParamParameters -- ^ Contains all available parameters of this operation (query and path parameters)
  -> OpenAPI.Common.ClientT m (Network.HTTP.Client.Types.Response MultiParamResponse) -- ^ Monadic computation which returns the result of the operation
multiParam parameters = GHC.Base.fmap (\response_0 -> GHC.Base.fmap (Data.Either.either MultiParamResponseError GHC.Base.id GHC.Base.. (\response body -> if | (\status_1 -> Network.HTTP.Types.Status.statusCode status_1 GHC.Classes.== 200) (Network.HTTP.Client.Types.responseStatus response) -> MultiParamResponse200 Data.Functor.<$> (Data.Aeson.Decoding.eitherDecodeStrict body :: Data.Either.Either GHC.Base.String
                                                                                                                                                                                                                                                                                                                                                                                                                Dog)
                                                                                                                                                             | GHC.Base.otherwise -> Data.Either.Left "Missing default response type") response_0) response_0) (OpenAPI.Common.doCallWithConfigurationM (Data.Text.toUpper GHC.Base.$ Data.Text.Internal.pack "GET") ("/pet/multiparam/" GHC.Base.<> (OpenAPI.Common.byteToText (Network.HTTP.Types.URI.urlEncode GHC.Types.True GHC.Base.$ (OpenAPI.Common.textToByte GHC.Base.$ OpenAPI.Common.stringifyModel (multiParamParametersPathStatus parameters))) GHC.Base.<> "")) [OpenAPI.Common.QueryParameter (Data.Text.Internal.pack "status") (GHC.Maybe.Just GHC.Base.$ Data.Aeson.Types.ToJSON.toJSON (multiParamParametersQueryStatus parameters)) (Data.Text.Internal.pack "form") GHC.Types.False,
                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                OpenAPI.Common.QueryParameter (Data.Text.Internal.pack "filter") (Data.Aeson.Types.ToJSON.toJSON Data.Functor.<$> multiParamParametersQueryFilter parameters) (Data.Text.Internal.pack "form") GHC.Types.False,
                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                OpenAPI.Common.QueryParameter (Data.Text.Internal.pack "referenceParameter") (GHC.Maybe.Just GHC.Base.$ Data.Aeson.Types.ToJSON.toJSON (multiParamParametersQueryReferenceParameter parameters)) (Data.Text.Internal.pack "form") GHC.Types.False])
-- | Defines the object schema located at @paths.\/pet\/multiparam\/{status}.GET.parameters@ in the specification.
-- 
-- 
data MultiParamParameters = MultiParamParameters {
  -- | pathStatus: Represents the parameter named \'status\'
  -- 
  -- Status in path
  multiParamParametersPathStatus :: MultiParamParametersPathStatus
  -- | queryFilter: Represents the parameter named \'filter\'
  -- 
  -- Filter the entries?
  , multiParamParametersQueryFilter :: (GHC.Maybe.Maybe GHC.Types.Bool)
  -- | queryReferenceParameter
  , multiParamParametersQueryReferenceParameter :: Cat
  -- | queryStatus: Represents the parameter named \'status\'
  -- 
  -- Status in query
  , multiParamParametersQueryStatus :: MultiParamParametersQueryStatus
  } deriving (GHC.Show.Show
  , GHC.Classes.Eq)
instance Data.Aeson.Types.ToJSON.ToJSON MultiParamParameters
    where {toJSON obj = Data.Aeson.Types.Internal.object (Data.Foldable.concat (["pathStatus" Data.Aeson.Types.ToJSON..= multiParamParametersPathStatus obj] : Data.Maybe.maybe GHC.Base.mempty (GHC.Base.pure GHC.Base.. ("queryFilter" Data.Aeson.Types.ToJSON..=)) (multiParamParametersQueryFilter obj) : ["queryReferenceParameter" Data.Aeson.Types.ToJSON..= multiParamParametersQueryReferenceParameter obj] : ["queryStatus" Data.Aeson.Types.ToJSON..= multiParamParametersQueryStatus obj] : GHC.Base.mempty));
           toEncoding obj = Data.Aeson.Encoding.Internal.pairs (GHC.Base.mconcat (Data.Foldable.concat (["pathStatus" Data.Aeson.Types.ToJSON..= multiParamParametersPathStatus obj] : Data.Maybe.maybe GHC.Base.mempty (GHC.Base.pure GHC.Base.. ("queryFilter" Data.Aeson.Types.ToJSON..=)) (multiParamParametersQueryFilter obj) : ["queryReferenceParameter" Data.Aeson.Types.ToJSON..= multiParamParametersQueryReferenceParameter obj] : ["queryStatus" Data.Aeson.Types.ToJSON..= multiParamParametersQueryStatus obj] : GHC.Base.mempty)))}
instance Data.Aeson.Types.FromJSON.FromJSON MultiParamParameters
    where {parseJSON = Data.Aeson.Types.FromJSON.withObject "MultiParamParameters" (\obj -> (((GHC.Base.pure MultiParamParameters GHC.Base.<*> (obj Data.Aeson.Types.FromJSON..: "pathStatus")) GHC.Base.<*> (obj Data.Aeson.Types.FromJSON..:! "queryFilter")) GHC.Base.<*> (obj Data.Aeson.Types.FromJSON..: "queryReferenceParameter")) GHC.Base.<*> (obj Data.Aeson.Types.FromJSON..: "queryStatus"))}
-- | Create a new 'MultiParamParameters' with all required fields.
mkMultiParamParameters :: MultiParamParametersPathStatus -- ^ 'multiParamParametersPathStatus'
  -> Cat -- ^ 'multiParamParametersQueryReferenceParameter'
  -> MultiParamParametersQueryStatus -- ^ 'multiParamParametersQueryStatus'
  -> MultiParamParameters
mkMultiParamParameters multiParamParametersPathStatus multiParamParametersQueryReferenceParameter multiParamParametersQueryStatus = MultiParamParameters{multiParamParametersPathStatus = multiParamParametersPathStatus,
                                                                                                                                                         multiParamParametersQueryFilter = GHC.Maybe.Nothing,
                                                                                                                                                         multiParamParametersQueryReferenceParameter = multiParamParametersQueryReferenceParameter,
                                                                                                                                                         multiParamParametersQueryStatus = multiParamParametersQueryStatus}
-- | Defines the enum schema located at @paths.\/pet\/multiparam\/{status}.GET.parameters.properties.pathStatus@ in the specification.
-- 
-- Represents the parameter named \'status\'
-- 
-- Status in path
data MultiParamParametersPathStatus =
   MultiParamParametersPathStatusOther Data.Aeson.Types.Internal.Value -- ^ This case is used if the value encountered during decoding does not match any of the provided cases in the specification.
  | MultiParamParametersPathStatusTyped GHC.Types.Int -- ^ This constructor can be used to send values to the server which are not present in the specification yet.
  | MultiParamParametersPathStatusEnum1 -- ^ Represents the JSON value @1@
  | MultiParamParametersPathStatusEnum3 -- ^ Represents the JSON value @3@
  | MultiParamParametersPathStatusEnum5 -- ^ Represents the JSON value @5@
  deriving (GHC.Show.Show, GHC.Classes.Eq)
instance Data.Aeson.Types.ToJSON.ToJSON MultiParamParametersPathStatus
    where {toJSON (MultiParamParametersPathStatusOther val) = val;
           toJSON (MultiParamParametersPathStatusTyped val) = Data.Aeson.Types.ToJSON.toJSON val;
           toJSON (MultiParamParametersPathStatusEnum1) = Data.Aeson.Types.Internal.Number (Data.Scientific.scientific 1 0);
           toJSON (MultiParamParametersPathStatusEnum3) = Data.Aeson.Types.Internal.Number (Data.Scientific.scientific 3 0);
           toJSON (MultiParamParametersPathStatusEnum5) = Data.Aeson.Types.Internal.Number (Data.Scientific.scientific 5 0)}
instance Data.Aeson.Types.FromJSON.FromJSON MultiParamParametersPathStatus
    where {parseJSON val = GHC.Base.pure (if | val GHC.Classes.== Data.Aeson.Types.Internal.Number (Data.Scientific.scientific 1 0) -> MultiParamParametersPathStatusEnum1
                                             | val GHC.Classes.== Data.Aeson.Types.Internal.Number (Data.Scientific.scientific 3 0) -> MultiParamParametersPathStatusEnum3
                                             | val GHC.Classes.== Data.Aeson.Types.Internal.Number (Data.Scientific.scientific 5 0) -> MultiParamParametersPathStatusEnum5
                                             | GHC.Base.otherwise -> MultiParamParametersPathStatusOther val)}
-- | Defines the enum schema located at @paths.\/pet\/multiparam\/{status}.GET.parameters.properties.queryStatus@ in the specification.
-- 
-- Represents the parameter named \'status\'
-- 
-- Status in query
data MultiParamParametersQueryStatus =
   MultiParamParametersQueryStatusOther Data.Aeson.Types.Internal.Value -- ^ This case is used if the value encountered during decoding does not match any of the provided cases in the specification.
  | MultiParamParametersQueryStatusTyped Data.Text.Internal.Text -- ^ This constructor can be used to send values to the server which are not present in the specification yet.
  | MultiParamParametersQueryStatusEnumAvailable -- ^ Represents the JSON value @"available"@
  | MultiParamParametersQueryStatusEnumPending -- ^ Represents the JSON value @"pending"@
  | MultiParamParametersQueryStatusEnumSold -- ^ Represents the JSON value @"sold"@
  deriving (GHC.Show.Show, GHC.Classes.Eq)
instance Data.Aeson.Types.ToJSON.ToJSON MultiParamParametersQueryStatus
    where {toJSON (MultiParamParametersQueryStatusOther val) = val;
           toJSON (MultiParamParametersQueryStatusTyped val) = Data.Aeson.Types.ToJSON.toJSON val;
           toJSON (MultiParamParametersQueryStatusEnumAvailable) = "available";
           toJSON (MultiParamParametersQueryStatusEnumPending) = "pending";
           toJSON (MultiParamParametersQueryStatusEnumSold) = "sold"}
instance Data.Aeson.Types.FromJSON.FromJSON MultiParamParametersQueryStatus
    where {parseJSON val = GHC.Base.pure (if | val GHC.Classes.== "available" -> MultiParamParametersQueryStatusEnumAvailable
                                             | val GHC.Classes.== "pending" -> MultiParamParametersQueryStatusEnumPending
                                             | val GHC.Classes.== "sold" -> MultiParamParametersQueryStatusEnumSold
                                             | GHC.Base.otherwise -> MultiParamParametersQueryStatusOther val)}
-- | Represents a response of the operation 'multiParam'.
-- 
-- The response constructor is chosen by the status code of the response. If no case matches (no specific case for the response code, no range case, no default case), 'MultiParamResponseError' is used.
data MultiParamResponse =
   MultiParamResponseError GHC.Base.String -- ^ Means either no matching case available or a parse error
  | MultiParamResponse200 Dog -- ^ successful operation
  deriving (GHC.Show.Show, GHC.Classes.Eq)
-- | > GET /pet/multiparam/{status}
-- 
-- The same as 'multiParam' but accepts an explicit configuration.
multiParamWithConfiguration :: forall m . OpenAPI.Common.MonadHTTP m => OpenAPI.Common.Configuration -- ^ The configuration to use in the request
  -> MultiParamParameters -- ^ Contains all available parameters of this operation (query and path parameters)
  -> m (Network.HTTP.Client.Types.Response MultiParamResponse) -- ^ Monadic computation which returns the result of the operation
multiParamWithConfiguration config
                            parameters = GHC.Base.fmap (\response_2 -> GHC.Base.fmap (Data.Either.either MultiParamResponseError GHC.Base.id GHC.Base.. (\response body -> if | (\status_3 -> Network.HTTP.Types.Status.statusCode status_3 GHC.Classes.== 200) (Network.HTTP.Client.Types.responseStatus response) -> MultiParamResponse200 Data.Functor.<$> (Data.Aeson.Decoding.eitherDecodeStrict body :: Data.Either.Either GHC.Base.String
                                                                                                                                                                                                                                                                                                                                                                                                                                 Dog)
                                                                                                                                                                              | GHC.Base.otherwise -> Data.Either.Left "Missing default response type") response_2) response_2) (OpenAPI.Common.doCallWithConfiguration config (Data.Text.toUpper GHC.Base.$ Data.Text.Internal.pack "GET") ("/pet/multiparam/" GHC.Base.<> (OpenAPI.Common.byteToText (Network.HTTP.Types.URI.urlEncode GHC.Types.True GHC.Base.$ (OpenAPI.Common.textToByte GHC.Base.$ OpenAPI.Common.stringifyModel (multiParamParametersPathStatus parameters))) GHC.Base.<> "")) [OpenAPI.Common.QueryParameter (Data.Text.Internal.pack "status") (GHC.Maybe.Just GHC.Base.$ Data.Aeson.Types.ToJSON.toJSON (multiParamParametersQueryStatus parameters)) (Data.Text.Internal.pack "form") GHC.Types.False,
                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                       OpenAPI.Common.QueryParameter (Data.Text.Internal.pack "filter") (Data.Aeson.Types.ToJSON.toJSON Data.Functor.<$> multiParamParametersQueryFilter parameters) (Data.Text.Internal.pack "form") GHC.Types.False,
                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                       OpenAPI.Common.QueryParameter (Data.Text.Internal.pack "referenceParameter") (GHC.Maybe.Just GHC.Base.$ Data.Aeson.Types.ToJSON.toJSON (multiParamParametersQueryReferenceParameter parameters)) (Data.Text.Internal.pack "form") GHC.Types.False])
-- | > GET /pet/multiparam/{status}
-- 
-- The same as 'multiParam' but returns the raw 'Data.ByteString.ByteString'.
multiParamRaw :: forall m . OpenAPI.Common.MonadHTTP m => MultiParamParameters -- ^ Contains all available parameters of this operation (query and path parameters)
  -> OpenAPI.Common.ClientT m (Network.HTTP.Client.Types.Response Data.ByteString.Internal.Type.ByteString) -- ^ Monadic computation which returns the result of the operation
multiParamRaw parameters = GHC.Base.id (OpenAPI.Common.doCallWithConfigurationM (Data.Text.toUpper GHC.Base.$ Data.Text.Internal.pack "GET") ("/pet/multiparam/" GHC.Base.<> (OpenAPI.Common.byteToText (Network.HTTP.Types.URI.urlEncode GHC.Types.True GHC.Base.$ (OpenAPI.Common.textToByte GHC.Base.$ OpenAPI.Common.stringifyModel (multiParamParametersPathStatus parameters))) GHC.Base.<> "")) [OpenAPI.Common.QueryParameter (Data.Text.Internal.pack "status") (GHC.Maybe.Just GHC.Base.$ Data.Aeson.Types.ToJSON.toJSON (multiParamParametersQueryStatus parameters)) (Data.Text.Internal.pack "form") GHC.Types.False,
                                                                                                                                                                                                                                                                                                                                                                                                        OpenAPI.Common.QueryParameter (Data.Text.Internal.pack "filter") (Data.Aeson.Types.ToJSON.toJSON Data.Functor.<$> multiParamParametersQueryFilter parameters) (Data.Text.Internal.pack "form") GHC.Types.False,
                                                                                                                                                                                                                                                                                                                                                                                                        OpenAPI.Common.QueryParameter (Data.Text.Internal.pack "referenceParameter") (GHC.Maybe.Just GHC.Base.$ Data.Aeson.Types.ToJSON.toJSON (multiParamParametersQueryReferenceParameter parameters)) (Data.Text.Internal.pack "form") GHC.Types.False])
-- | > GET /pet/multiparam/{status}
-- 
-- The same as 'multiParam' but accepts an explicit configuration and returns the raw 'Data.ByteString.ByteString'.
multiParamWithConfigurationRaw :: forall m . OpenAPI.Common.MonadHTTP m => OpenAPI.Common.Configuration -- ^ The configuration to use in the request
  -> MultiParamParameters -- ^ Contains all available parameters of this operation (query and path parameters)
  -> m (Network.HTTP.Client.Types.Response Data.ByteString.Internal.Type.ByteString) -- ^ Monadic computation which returns the result of the operation
multiParamWithConfigurationRaw config
                               parameters = GHC.Base.id (OpenAPI.Common.doCallWithConfiguration config (Data.Text.toUpper GHC.Base.$ Data.Text.Internal.pack "GET") ("/pet/multiparam/" GHC.Base.<> (OpenAPI.Common.byteToText (Network.HTTP.Types.URI.urlEncode GHC.Types.True GHC.Base.$ (OpenAPI.Common.textToByte GHC.Base.$ OpenAPI.Common.stringifyModel (multiParamParametersPathStatus parameters))) GHC.Base.<> "")) [OpenAPI.Common.QueryParameter (Data.Text.Internal.pack "status") (GHC.Maybe.Just GHC.Base.$ Data.Aeson.Types.ToJSON.toJSON (multiParamParametersQueryStatus parameters)) (Data.Text.Internal.pack "form") GHC.Types.False,
                                                                                                                                                                                                                                                                                                                                                                                                                               OpenAPI.Common.QueryParameter (Data.Text.Internal.pack "filter") (Data.Aeson.Types.ToJSON.toJSON Data.Functor.<$> multiParamParametersQueryFilter parameters) (Data.Text.Internal.pack "form") GHC.Types.False,
                                                                                                                                                                                                                                                                                                                                                                                                                               OpenAPI.Common.QueryParameter (Data.Text.Internal.pack "referenceParameter") (GHC.Maybe.Just GHC.Base.$ Data.Aeson.Types.ToJSON.toJSON (multiParamParametersQueryReferenceParameter parameters)) (Data.Text.Internal.pack "form") GHC.Types.False])
