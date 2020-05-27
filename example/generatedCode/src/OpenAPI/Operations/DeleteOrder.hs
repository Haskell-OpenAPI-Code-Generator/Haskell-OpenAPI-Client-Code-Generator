{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ExplicitForAll #-}
{-# LANGUAGE MultiWayIf #-}
{-# LANGUAGE DeriveGeneric #-}

-- | Contains the different functions to run the operation deleteOrder
module OpenAPI.Operations.DeleteOrder where

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

-- | > DELETE /store/order/{orderId}
-- 
-- For valid response try integer IDs with positive integer value. Negative or non-integer values will generate API errors
deleteOrder :: forall m s . (OpenAPI.Common.MonadHTTP m, OpenAPI.Common.SecurityScheme s) => OpenAPI.Common.Configuration s  -- ^ The configuration to use in the request
  -> GHC.Int.Int64                                                                                                           -- ^ orderId: ID of the order that needs to be deleted | Constraints: Minimum  of 1.0
  -> m (Data.Either.Either Network.HTTP.Client.Types.HttpException (Network.HTTP.Client.Types.Response DeleteOrderResponse)) -- ^ Monad containing the result of the operation
deleteOrder config
            orderId = GHC.Base.fmap (GHC.Base.fmap (\response_0 -> GHC.Base.fmap (Data.Either.either DeleteOrderResponseError GHC.Base.id GHC.Base.. (\response body -> if | (\status_1 -> Network.HTTP.Types.Status.statusCode status_1 GHC.Classes.== 400) (Network.HTTP.Client.Types.responseStatus response) -> Data.Either.Right DeleteOrderResponse400
                                                                                                                                                                           | (\status_2 -> Network.HTTP.Types.Status.statusCode status_2 GHC.Classes.== 404) (Network.HTTP.Client.Types.responseStatus response) -> Data.Either.Right DeleteOrderResponse404
                                                                                                                                                                           | GHC.Base.otherwise -> Data.Either.Left "Missing default response type") response_0) response_0)) (OpenAPI.Common.doCallWithConfiguration config (Data.Text.toUpper (Data.Text.pack "DELETE")) (Data.Text.pack ("/store/order/" GHC.Base.++ (Data.ByteString.Char8.unpack (Network.HTTP.Types.URI.urlEncode GHC.Types.True GHC.Base.$ (Data.ByteString.Char8.pack GHC.Base.$ OpenAPI.Common.stringifyModel orderId)) GHC.Base.++ ""))) [])
-- | > DELETE /store/order/{orderId}
-- 
-- The same as 'deleteOrder' but returns the raw 'Data.ByteString.Char8.ByteString'
deleteOrderRaw :: forall m s . (OpenAPI.Common.MonadHTTP m,
                                OpenAPI.Common.SecurityScheme s) =>
                  OpenAPI.Common.Configuration s ->
                  GHC.Int.Int64 ->
                  m (Data.Either.Either Network.HTTP.Client.Types.HttpException
                                        (Network.HTTP.Client.Types.Response Data.ByteString.Internal.ByteString))
deleteOrderRaw config
               orderId = GHC.Base.id (OpenAPI.Common.doCallWithConfiguration config (Data.Text.toUpper (Data.Text.pack "DELETE")) (Data.Text.pack ("/store/order/" GHC.Base.++ (Data.ByteString.Char8.unpack (Network.HTTP.Types.URI.urlEncode GHC.Types.True GHC.Base.$ (Data.ByteString.Char8.pack GHC.Base.$ OpenAPI.Common.stringifyModel orderId)) GHC.Base.++ ""))) [])
-- | > DELETE /store/order/{orderId}
-- 
-- Monadic version of 'deleteOrder' (use with 'OpenAPI.Common.runWithConfiguration')
deleteOrderM :: forall m s . (OpenAPI.Common.MonadHTTP m,
                              OpenAPI.Common.SecurityScheme s) =>
                GHC.Int.Int64 ->
                Control.Monad.Trans.Reader.ReaderT (OpenAPI.Common.Configuration s)
                                                   m
                                                   (Data.Either.Either Network.HTTP.Client.Types.HttpException
                                                                       (Network.HTTP.Client.Types.Response DeleteOrderResponse))
deleteOrderM orderId = GHC.Base.fmap (GHC.Base.fmap (\response_3 -> GHC.Base.fmap (Data.Either.either DeleteOrderResponseError GHC.Base.id GHC.Base.. (\response body -> if | (\status_4 -> Network.HTTP.Types.Status.statusCode status_4 GHC.Classes.== 400) (Network.HTTP.Client.Types.responseStatus response) -> Data.Either.Right DeleteOrderResponse400
                                                                                                                                                                            | (\status_5 -> Network.HTTP.Types.Status.statusCode status_5 GHC.Classes.== 404) (Network.HTTP.Client.Types.responseStatus response) -> Data.Either.Right DeleteOrderResponse404
                                                                                                                                                                            | GHC.Base.otherwise -> Data.Either.Left "Missing default response type") response_3) response_3)) (OpenAPI.Common.doCallWithConfigurationM (Data.Text.toUpper (Data.Text.pack "DELETE")) (Data.Text.pack ("/store/order/" GHC.Base.++ (Data.ByteString.Char8.unpack (Network.HTTP.Types.URI.urlEncode GHC.Types.True GHC.Base.$ (Data.ByteString.Char8.pack GHC.Base.$ OpenAPI.Common.stringifyModel orderId)) GHC.Base.++ ""))) [])
-- | > DELETE /store/order/{orderId}
-- 
-- Monadic version of 'deleteOrderRaw' (use with 'OpenAPI.Common.runWithConfiguration')
deleteOrderRawM :: forall m s . (OpenAPI.Common.MonadHTTP m,
                                 OpenAPI.Common.SecurityScheme s) =>
                   GHC.Int.Int64 ->
                   Control.Monad.Trans.Reader.ReaderT (OpenAPI.Common.Configuration s)
                                                      m
                                                      (Data.Either.Either Network.HTTP.Client.Types.HttpException
                                                                          (Network.HTTP.Client.Types.Response Data.ByteString.Internal.ByteString))
deleteOrderRawM orderId = GHC.Base.id (OpenAPI.Common.doCallWithConfigurationM (Data.Text.toUpper (Data.Text.pack "DELETE")) (Data.Text.pack ("/store/order/" GHC.Base.++ (Data.ByteString.Char8.unpack (Network.HTTP.Types.URI.urlEncode GHC.Types.True GHC.Base.$ (Data.ByteString.Char8.pack GHC.Base.$ OpenAPI.Common.stringifyModel orderId)) GHC.Base.++ ""))) [])
-- | Represents a response of the operation 'deleteOrder'.
-- 
-- The response constructor is chosen by the status code of the response. If no case matches (no specific case for the response code, no range case, no default case), 'DeleteOrderResponseError' is used.
data DeleteOrderResponse =                   
   DeleteOrderResponseError GHC.Base.String  -- ^ Means either no matching case available or a parse error
  | DeleteOrderResponse400                   -- ^ Invalid ID supplied
  | DeleteOrderResponse404                   -- ^ Order not found
  deriving (GHC.Show.Show, GHC.Classes.Eq)
