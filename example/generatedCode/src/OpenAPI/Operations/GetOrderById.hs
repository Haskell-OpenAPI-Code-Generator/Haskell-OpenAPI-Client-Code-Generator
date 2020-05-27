{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ExplicitForAll #-}
{-# LANGUAGE MultiWayIf #-}
{-# LANGUAGE DeriveGeneric #-}

-- | Contains the different functions to run the operation getOrderById
module OpenAPI.Operations.GetOrderById where

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

-- | > GET /store/order/{orderId}
-- 
-- For valid response try integer IDs with value >= 1 and \<= 10. Other values will generated exceptions
getOrderById :: forall m s . (OpenAPI.Common.MonadHTTP m, OpenAPI.Common.SecurityScheme s) => OpenAPI.Common.Configuration s  -- ^ The configuration to use in the request
  -> GHC.Int.Int64                                                                                                            -- ^ orderId: ID of pet that needs to be fetched | Constraints: Maxium  of 10.0, Minimum  of 1.0
  -> m (Data.Either.Either Network.HTTP.Client.Types.HttpException (Network.HTTP.Client.Types.Response GetOrderByIdResponse)) -- ^ Monad containing the result of the operation
getOrderById config
             orderId = GHC.Base.fmap (GHC.Base.fmap (\response_0 -> GHC.Base.fmap (Data.Either.either GetOrderByIdResponseError GHC.Base.id GHC.Base.. (\response body -> if | (\status_1 -> Network.HTTP.Types.Status.statusCode status_1 GHC.Classes.== 200) (Network.HTTP.Client.Types.responseStatus response) -> GetOrderByIdResponse200 Data.Functor.<$> (Data.Aeson.eitherDecodeStrict body :: Data.Either.Either GHC.Base.String
                                                                                                                                                                                                                                                                                                                                                                                                                         Order)
                                                                                                                                                                             | (\status_2 -> Network.HTTP.Types.Status.statusCode status_2 GHC.Classes.== 400) (Network.HTTP.Client.Types.responseStatus response) -> Data.Either.Right GetOrderByIdResponse400
                                                                                                                                                                             | (\status_3 -> Network.HTTP.Types.Status.statusCode status_3 GHC.Classes.== 404) (Network.HTTP.Client.Types.responseStatus response) -> Data.Either.Right GetOrderByIdResponse404
                                                                                                                                                                             | GHC.Base.otherwise -> Data.Either.Left "Missing default response type") response_0) response_0)) (OpenAPI.Common.doCallWithConfiguration config (Data.Text.toUpper (Data.Text.pack "GET")) (Data.Text.pack ("/store/order/" GHC.Base.++ (Data.ByteString.Char8.unpack (Network.HTTP.Types.URI.urlEncode GHC.Types.True GHC.Base.$ (Data.ByteString.Char8.pack GHC.Base.$ OpenAPI.Common.stringifyModel orderId)) GHC.Base.++ ""))) [])
-- | > GET /store/order/{orderId}
-- 
-- The same as 'getOrderById' but returns the raw 'Data.ByteString.Char8.ByteString'
getOrderByIdRaw :: forall m s . (OpenAPI.Common.MonadHTTP m,
                                 OpenAPI.Common.SecurityScheme s) =>
                   OpenAPI.Common.Configuration s ->
                   GHC.Int.Int64 ->
                   m (Data.Either.Either Network.HTTP.Client.Types.HttpException
                                         (Network.HTTP.Client.Types.Response Data.ByteString.Internal.ByteString))
getOrderByIdRaw config
                orderId = GHC.Base.id (OpenAPI.Common.doCallWithConfiguration config (Data.Text.toUpper (Data.Text.pack "GET")) (Data.Text.pack ("/store/order/" GHC.Base.++ (Data.ByteString.Char8.unpack (Network.HTTP.Types.URI.urlEncode GHC.Types.True GHC.Base.$ (Data.ByteString.Char8.pack GHC.Base.$ OpenAPI.Common.stringifyModel orderId)) GHC.Base.++ ""))) [])
-- | > GET /store/order/{orderId}
-- 
-- Monadic version of 'getOrderById' (use with 'OpenAPI.Common.runWithConfiguration')
getOrderByIdM :: forall m s . (OpenAPI.Common.MonadHTTP m,
                               OpenAPI.Common.SecurityScheme s) =>
                 GHC.Int.Int64 ->
                 Control.Monad.Trans.Reader.ReaderT (OpenAPI.Common.Configuration s)
                                                    m
                                                    (Data.Either.Either Network.HTTP.Client.Types.HttpException
                                                                        (Network.HTTP.Client.Types.Response GetOrderByIdResponse))
getOrderByIdM orderId = GHC.Base.fmap (GHC.Base.fmap (\response_4 -> GHC.Base.fmap (Data.Either.either GetOrderByIdResponseError GHC.Base.id GHC.Base.. (\response body -> if | (\status_5 -> Network.HTTP.Types.Status.statusCode status_5 GHC.Classes.== 200) (Network.HTTP.Client.Types.responseStatus response) -> GetOrderByIdResponse200 Data.Functor.<$> (Data.Aeson.eitherDecodeStrict body :: Data.Either.Either GHC.Base.String
                                                                                                                                                                                                                                                                                                                                                                                                                          Order)
                                                                                                                                                                              | (\status_6 -> Network.HTTP.Types.Status.statusCode status_6 GHC.Classes.== 400) (Network.HTTP.Client.Types.responseStatus response) -> Data.Either.Right GetOrderByIdResponse400
                                                                                                                                                                              | (\status_7 -> Network.HTTP.Types.Status.statusCode status_7 GHC.Classes.== 404) (Network.HTTP.Client.Types.responseStatus response) -> Data.Either.Right GetOrderByIdResponse404
                                                                                                                                                                              | GHC.Base.otherwise -> Data.Either.Left "Missing default response type") response_4) response_4)) (OpenAPI.Common.doCallWithConfigurationM (Data.Text.toUpper (Data.Text.pack "GET")) (Data.Text.pack ("/store/order/" GHC.Base.++ (Data.ByteString.Char8.unpack (Network.HTTP.Types.URI.urlEncode GHC.Types.True GHC.Base.$ (Data.ByteString.Char8.pack GHC.Base.$ OpenAPI.Common.stringifyModel orderId)) GHC.Base.++ ""))) [])
-- | > GET /store/order/{orderId}
-- 
-- Monadic version of 'getOrderByIdRaw' (use with 'OpenAPI.Common.runWithConfiguration')
getOrderByIdRawM :: forall m s . (OpenAPI.Common.MonadHTTP m,
                                  OpenAPI.Common.SecurityScheme s) =>
                    GHC.Int.Int64 ->
                    Control.Monad.Trans.Reader.ReaderT (OpenAPI.Common.Configuration s)
                                                       m
                                                       (Data.Either.Either Network.HTTP.Client.Types.HttpException
                                                                           (Network.HTTP.Client.Types.Response Data.ByteString.Internal.ByteString))
getOrderByIdRawM orderId = GHC.Base.id (OpenAPI.Common.doCallWithConfigurationM (Data.Text.toUpper (Data.Text.pack "GET")) (Data.Text.pack ("/store/order/" GHC.Base.++ (Data.ByteString.Char8.unpack (Network.HTTP.Types.URI.urlEncode GHC.Types.True GHC.Base.$ (Data.ByteString.Char8.pack GHC.Base.$ OpenAPI.Common.stringifyModel orderId)) GHC.Base.++ ""))) [])
-- | Represents a response of the operation 'getOrderById'.
-- 
-- The response constructor is chosen by the status code of the response. If no case matches (no specific case for the response code, no range case, no default case), 'GetOrderByIdResponseError' is used.
data GetOrderByIdResponse =                   
   GetOrderByIdResponseError GHC.Base.String  -- ^ Means either no matching case available or a parse error
  | GetOrderByIdResponse200 Order             -- ^ successful operation
  | GetOrderByIdResponse400                   -- ^ Invalid ID supplied
  | GetOrderByIdResponse404                   -- ^ Order not found
  deriving (GHC.Show.Show, GHC.Classes.Eq)
