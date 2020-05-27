{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ExplicitForAll #-}
{-# LANGUAGE MultiWayIf #-}
{-# LANGUAGE DeriveGeneric #-}

-- | Contains the different functions to run the operation updateUser
module OpenAPI.Operations.UpdateUser where

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

-- | > PUT /user/{username}
-- 
-- This can only be done by the logged in user.
updateUser :: forall m s . (OpenAPI.Common.MonadHTTP m, OpenAPI.Common.SecurityScheme s) => OpenAPI.Common.Configuration s  -- ^ The configuration to use in the request
  -> GHC.Base.String                                                                                                        -- ^ username: name that need to be updated
  -> User                                                                                                                   -- ^ The request body to send
  -> m (Data.Either.Either Network.HTTP.Client.Types.HttpException (Network.HTTP.Client.Types.Response UpdateUserResponse)) -- ^ Monad containing the result of the operation
updateUser config
           username
           body = GHC.Base.fmap (GHC.Base.fmap (\response_0 -> GHC.Base.fmap (Data.Either.either UpdateUserResponseError GHC.Base.id GHC.Base.. (\response body -> if | (\status_1 -> Network.HTTP.Types.Status.statusCode status_1 GHC.Classes.== 400) (Network.HTTP.Client.Types.responseStatus response) -> Data.Either.Right UpdateUserResponse400
                                                                                                                                                                      | (\status_2 -> Network.HTTP.Types.Status.statusCode status_2 GHC.Classes.== 404) (Network.HTTP.Client.Types.responseStatus response) -> Data.Either.Right UpdateUserResponse404
                                                                                                                                                                      | GHC.Base.otherwise -> Data.Either.Left "Missing default response type") response_0) response_0)) (OpenAPI.Common.doBodyCallWithConfiguration config (Data.Text.toUpper (Data.Text.pack "PUT")) (Data.Text.pack ("/user/" GHC.Base.++ (Data.ByteString.Char8.unpack (Network.HTTP.Types.URI.urlEncode GHC.Types.True GHC.Base.$ (Data.ByteString.Char8.pack GHC.Base.$ OpenAPI.Common.stringifyModel username)) GHC.Base.++ ""))) [] body OpenAPI.Common.RequestBodyEncodingJSON)
-- | > PUT /user/{username}
-- 
-- The same as 'updateUser' but returns the raw 'Data.ByteString.Char8.ByteString'
updateUserRaw :: forall m s . (OpenAPI.Common.MonadHTTP m,
                               OpenAPI.Common.SecurityScheme s) =>
                 OpenAPI.Common.Configuration s ->
                 GHC.Base.String ->
                 User ->
                 m (Data.Either.Either Network.HTTP.Client.Types.HttpException
                                       (Network.HTTP.Client.Types.Response Data.ByteString.Internal.ByteString))
updateUserRaw config
              username
              body = GHC.Base.id (OpenAPI.Common.doBodyCallWithConfiguration config (Data.Text.toUpper (Data.Text.pack "PUT")) (Data.Text.pack ("/user/" GHC.Base.++ (Data.ByteString.Char8.unpack (Network.HTTP.Types.URI.urlEncode GHC.Types.True GHC.Base.$ (Data.ByteString.Char8.pack GHC.Base.$ OpenAPI.Common.stringifyModel username)) GHC.Base.++ ""))) [] body OpenAPI.Common.RequestBodyEncodingJSON)
-- | > PUT /user/{username}
-- 
-- Monadic version of 'updateUser' (use with 'OpenAPI.Common.runWithConfiguration')
updateUserM :: forall m s . (OpenAPI.Common.MonadHTTP m,
                             OpenAPI.Common.SecurityScheme s) =>
               GHC.Base.String ->
               User ->
               Control.Monad.Trans.Reader.ReaderT (OpenAPI.Common.Configuration s)
                                                  m
                                                  (Data.Either.Either Network.HTTP.Client.Types.HttpException
                                                                      (Network.HTTP.Client.Types.Response UpdateUserResponse))
updateUserM username
            body = GHC.Base.fmap (GHC.Base.fmap (\response_3 -> GHC.Base.fmap (Data.Either.either UpdateUserResponseError GHC.Base.id GHC.Base.. (\response body -> if | (\status_4 -> Network.HTTP.Types.Status.statusCode status_4 GHC.Classes.== 400) (Network.HTTP.Client.Types.responseStatus response) -> Data.Either.Right UpdateUserResponse400
                                                                                                                                                                       | (\status_5 -> Network.HTTP.Types.Status.statusCode status_5 GHC.Classes.== 404) (Network.HTTP.Client.Types.responseStatus response) -> Data.Either.Right UpdateUserResponse404
                                                                                                                                                                       | GHC.Base.otherwise -> Data.Either.Left "Missing default response type") response_3) response_3)) (OpenAPI.Common.doBodyCallWithConfigurationM (Data.Text.toUpper (Data.Text.pack "PUT")) (Data.Text.pack ("/user/" GHC.Base.++ (Data.ByteString.Char8.unpack (Network.HTTP.Types.URI.urlEncode GHC.Types.True GHC.Base.$ (Data.ByteString.Char8.pack GHC.Base.$ OpenAPI.Common.stringifyModel username)) GHC.Base.++ ""))) [] body OpenAPI.Common.RequestBodyEncodingJSON)
-- | > PUT /user/{username}
-- 
-- Monadic version of 'updateUserRaw' (use with 'OpenAPI.Common.runWithConfiguration')
updateUserRawM :: forall m s . (OpenAPI.Common.MonadHTTP m,
                                OpenAPI.Common.SecurityScheme s) =>
                  GHC.Base.String ->
                  User ->
                  Control.Monad.Trans.Reader.ReaderT (OpenAPI.Common.Configuration s)
                                                     m
                                                     (Data.Either.Either Network.HTTP.Client.Types.HttpException
                                                                         (Network.HTTP.Client.Types.Response Data.ByteString.Internal.ByteString))
updateUserRawM username
               body = GHC.Base.id (OpenAPI.Common.doBodyCallWithConfigurationM (Data.Text.toUpper (Data.Text.pack "PUT")) (Data.Text.pack ("/user/" GHC.Base.++ (Data.ByteString.Char8.unpack (Network.HTTP.Types.URI.urlEncode GHC.Types.True GHC.Base.$ (Data.ByteString.Char8.pack GHC.Base.$ OpenAPI.Common.stringifyModel username)) GHC.Base.++ ""))) [] body OpenAPI.Common.RequestBodyEncodingJSON)
-- | Represents a response of the operation 'updateUser'.
-- 
-- The response constructor is chosen by the status code of the response. If no case matches (no specific case for the response code, no range case, no default case), 'UpdateUserResponseError' is used.
data UpdateUserResponse =                   
   UpdateUserResponseError GHC.Base.String  -- ^ Means either no matching case available or a parse error
  | UpdateUserResponse400                   -- ^ Invalid user supplied
  | UpdateUserResponse404                   -- ^ User not found
  deriving (GHC.Show.Show, GHC.Classes.Eq)
