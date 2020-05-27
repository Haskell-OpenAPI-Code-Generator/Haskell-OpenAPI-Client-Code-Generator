{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ExplicitForAll #-}
{-# LANGUAGE MultiWayIf #-}
{-# LANGUAGE DeriveGeneric #-}

-- | Contains the different functions to run the operation getInventory
module OpenAPI.Operations.GetInventory where

import qualified Prelude as GHC.Integer.Type
import qualified Prelude as GHC.Maybe
import qualified Control.Monad.Trans.Reader
import qualified Data.Aeson
import qualified Data.Aeson as Data.Aeson.Types
import qualified Data.Aeson as Data.Aeson.Types.FromJSON
import qualified Data.Aeson as Data.Aeson.Types.ToJSON
import qualified Data.Aeson as Data.Aeson.Types.Internal
import qualified Data.ByteString.Char8
import qualified Data.ByteString.Char8 as Data.ByteString.Internal
import qualified Data.Either
import qualified Data.Functor
import qualified Data.Scientific
import qualified Data.Text
import qualified Data.Text.Internal
import qualified Data.Time.Calendar as Data.Time.Calendar.Days
import qualified Data.Time.LocalTime as Data.Time.LocalTime.Internal.ZonedTime
import qualified GHC.Base
import qualified GHC.Classes
import qualified GHC.Generics
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

-- | > GET /store/inventory
-- 
-- Returns a map of status codes to quantities
getInventory :: forall m s . (OpenAPI.Common.MonadHTTP m, OpenAPI.Common.SecurityScheme s) => OpenAPI.Common.Configuration s  -- ^ The configuration to use in the request
  -> m (Data.Either.Either Network.HTTP.Client.Types.HttpException (Network.HTTP.Client.Types.Response GetInventoryResponse)) -- ^ Monad containing the result of the operation
getInventory config = GHC.Base.fmap (GHC.Base.fmap (\response_0 -> GHC.Base.fmap (Data.Either.either GetInventoryResponseError GHC.Base.id GHC.Base.. (\response body -> if | (\status_1 -> Network.HTTP.Types.Status.statusCode status_1 GHC.Classes.== 200) (Network.HTTP.Client.Types.responseStatus response) -> GetInventoryResponse200 Data.Functor.<$> (Data.Aeson.eitherDecodeStrict body :: Data.Either.Either GHC.Base.String
                                                                                                                                                                                                                                                                                                                                                                                                                        GetInventoryResponseBody200)
                                                                                                                                                                            | GHC.Base.otherwise -> Data.Either.Left "Missing default response type") response_0) response_0)) (OpenAPI.Common.doCallWithConfiguration config (Data.Text.toUpper (Data.Text.pack "GET")) (Data.Text.pack "/store/inventory") [])
-- | > GET /store/inventory
-- 
-- The same as 'getInventory' but returns the raw 'Data.ByteString.Char8.ByteString'
getInventoryRaw :: forall m s . (OpenAPI.Common.MonadHTTP m,
                                 OpenAPI.Common.SecurityScheme s) =>
                   OpenAPI.Common.Configuration s ->
                   m (Data.Either.Either Network.HTTP.Client.Types.HttpException
                                         (Network.HTTP.Client.Types.Response Data.ByteString.Internal.ByteString))
getInventoryRaw config = GHC.Base.id (OpenAPI.Common.doCallWithConfiguration config (Data.Text.toUpper (Data.Text.pack "GET")) (Data.Text.pack "/store/inventory") [])
-- | > GET /store/inventory
-- 
-- Monadic version of 'getInventory' (use with 'OpenAPI.Common.runWithConfiguration')
getInventoryM :: forall m s . (OpenAPI.Common.MonadHTTP m,
                               OpenAPI.Common.SecurityScheme s) =>
                 Control.Monad.Trans.Reader.ReaderT (OpenAPI.Common.Configuration s)
                                                    m
                                                    (Data.Either.Either Network.HTTP.Client.Types.HttpException
                                                                        (Network.HTTP.Client.Types.Response GetInventoryResponse))
getInventoryM = GHC.Base.fmap (GHC.Base.fmap (\response_2 -> GHC.Base.fmap (Data.Either.either GetInventoryResponseError GHC.Base.id GHC.Base.. (\response body -> if | (\status_3 -> Network.HTTP.Types.Status.statusCode status_3 GHC.Classes.== 200) (Network.HTTP.Client.Types.responseStatus response) -> GetInventoryResponse200 Data.Functor.<$> (Data.Aeson.eitherDecodeStrict body :: Data.Either.Either GHC.Base.String
                                                                                                                                                                                                                                                                                                                                                                                                                  GetInventoryResponseBody200)
                                                                                                                                                                      | GHC.Base.otherwise -> Data.Either.Left "Missing default response type") response_2) response_2)) (OpenAPI.Common.doCallWithConfigurationM (Data.Text.toUpper (Data.Text.pack "GET")) (Data.Text.pack "/store/inventory") [])
-- | > GET /store/inventory
-- 
-- Monadic version of 'getInventoryRaw' (use with 'OpenAPI.Common.runWithConfiguration')
getInventoryRawM :: forall m s . (OpenAPI.Common.MonadHTTP m,
                                  OpenAPI.Common.SecurityScheme s) =>
                    Control.Monad.Trans.Reader.ReaderT (OpenAPI.Common.Configuration s)
                                                       m
                                                       (Data.Either.Either Network.HTTP.Client.Types.HttpException
                                                                           (Network.HTTP.Client.Types.Response Data.ByteString.Internal.ByteString))
getInventoryRawM = GHC.Base.id (OpenAPI.Common.doCallWithConfigurationM (Data.Text.toUpper (Data.Text.pack "GET")) (Data.Text.pack "/store/inventory") [])
-- | Represents a response of the operation 'getInventory'.
-- 
-- The response constructor is chosen by the status code of the response. If no case matches (no specific case for the response code, no range case, no default case), 'GetInventoryResponseError' is used.
data GetInventoryResponse =                              
   GetInventoryResponseError GHC.Base.String             -- ^ Means either no matching case available or a parse error
  | GetInventoryResponse200 GetInventoryResponseBody200  -- ^ successful operation
  deriving (GHC.Show.Show, GHC.Classes.Eq)
-- | Defines the data type for the schema GetInventoryResponseBody200
-- 
-- 
data GetInventoryResponseBody200 = GetInventoryResponseBody200 {
  
  } deriving (GHC.Show.Show
  , GHC.Classes.Eq)
instance Data.Aeson.ToJSON GetInventoryResponseBody200
    where toJSON obj = Data.Aeson.object []
          toEncoding obj = Data.Aeson.pairs ((Data.Aeson..=) "string" ("string" :: GHC.Base.String))
instance Data.Aeson.Types.FromJSON.FromJSON GetInventoryResponseBody200
    where parseJSON = Data.Aeson.Types.FromJSON.withObject "GetInventoryResponseBody200" (\obj -> GHC.Base.pure GetInventoryResponseBody200)
