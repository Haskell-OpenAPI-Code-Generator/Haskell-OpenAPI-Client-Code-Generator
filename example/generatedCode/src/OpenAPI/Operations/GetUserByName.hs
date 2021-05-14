{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE ExplicitForAll #-}
{-# LANGUAGE MultiWayIf #-}
{-# LANGUAGE OverloadedStrings #-}

-- | Contains the different functions to run the operation getUserByName
module OpenAPI.Operations.GetUserByName where

import qualified Control.Monad.Trans.Reader
import qualified Data.Aeson
import qualified Data.Aeson as Data.Aeson.Types
import qualified Data.Aeson as Data.Aeson.Types.FromJSON
import qualified Data.Aeson as Data.Aeson.Types.Internal
import qualified Data.Aeson as Data.Aeson.Types.ToJSON
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
import qualified Prelude as GHC.Integer.Type
import qualified Prelude as GHC.Maybe

-- | > GET /user/{username}
getUserByName ::
  forall m s.
  (OpenAPI.Common.MonadHTTP m, OpenAPI.Common.SecurityScheme s) =>
  -- | The configuration to use in the request
  OpenAPI.Common.Configuration s ->
  -- | username: The name that needs to be fetched. Use user1 for testing.
  GHC.Base.String ->
  -- | Monad containing the result of the operation
  m (Data.Either.Either Network.HTTP.Client.Types.HttpException (Network.HTTP.Client.Types.Response GetUserByNameResponse))
getUserByName
  config
  username =
    GHC.Base.fmap
      ( GHC.Base.fmap
          ( \response_0 ->
              GHC.Base.fmap
                ( Data.Either.either GetUserByNameResponseError GHC.Base.id
                    GHC.Base.. ( \response body ->
                                   if
                                       | (\status_1 -> Network.HTTP.Types.Status.statusCode status_1 GHC.Classes.== 200) (Network.HTTP.Client.Types.responseStatus response) ->
                                         GetUserByNameResponse200
                                           Data.Functor.<$> ( Data.Aeson.eitherDecodeStrict body ::
                                                                Data.Either.Either
                                                                  GHC.Base.String
                                                                  User
                                                            )
                                       | (\status_2 -> Network.HTTP.Types.Status.statusCode status_2 GHC.Classes.== 400) (Network.HTTP.Client.Types.responseStatus response) -> Data.Either.Right GetUserByNameResponse400
                                       | (\status_3 -> Network.HTTP.Types.Status.statusCode status_3 GHC.Classes.== 404) (Network.HTTP.Client.Types.responseStatus response) -> Data.Either.Right GetUserByNameResponse404
                                       | GHC.Base.otherwise -> Data.Either.Left "Missing default response type"
                               )
                      response_0
                )
                response_0
          )
      )
      (OpenAPI.Common.doCallWithConfiguration config (Data.Text.toUpper (Data.Text.pack "GET")) (Data.Text.pack ("/user/" GHC.Base.++ (Data.ByteString.Char8.unpack (Network.HTTP.Types.URI.urlEncode GHC.Types.True GHC.Base.$ (Data.ByteString.Char8.pack GHC.Base.$ OpenAPI.Common.stringifyModel username)) GHC.Base.++ ""))) [])

-- | > GET /user/{username}
--
-- The same as 'getUserByName' but returns the raw 'Data.ByteString.Char8.ByteString'
getUserByNameRaw ::
  forall m s.
  ( OpenAPI.Common.MonadHTTP m,
    OpenAPI.Common.SecurityScheme s
  ) =>
  OpenAPI.Common.Configuration s ->
  GHC.Base.String ->
  m
    ( Data.Either.Either
        Network.HTTP.Client.Types.HttpException
        (Network.HTTP.Client.Types.Response Data.ByteString.Internal.ByteString)
    )
getUserByNameRaw
  config
  username = GHC.Base.id (OpenAPI.Common.doCallWithConfiguration config (Data.Text.toUpper (Data.Text.pack "GET")) (Data.Text.pack ("/user/" GHC.Base.++ (Data.ByteString.Char8.unpack (Network.HTTP.Types.URI.urlEncode GHC.Types.True GHC.Base.$ (Data.ByteString.Char8.pack GHC.Base.$ OpenAPI.Common.stringifyModel username)) GHC.Base.++ ""))) [])

-- | > GET /user/{username}
--
-- Monadic version of 'getUserByName' (use with 'OpenAPI.Common.runWithConfiguration')
getUserByNameM ::
  forall m s.
  ( OpenAPI.Common.MonadHTTP m,
    OpenAPI.Common.SecurityScheme s
  ) =>
  GHC.Base.String ->
  Control.Monad.Trans.Reader.ReaderT
    (OpenAPI.Common.Configuration s)
    m
    ( Data.Either.Either
        Network.HTTP.Client.Types.HttpException
        (Network.HTTP.Client.Types.Response GetUserByNameResponse)
    )
getUserByNameM username =
  GHC.Base.fmap
    ( GHC.Base.fmap
        ( \response_4 ->
            GHC.Base.fmap
              ( Data.Either.either GetUserByNameResponseError GHC.Base.id
                  GHC.Base.. ( \response body ->
                                 if
                                     | (\status_5 -> Network.HTTP.Types.Status.statusCode status_5 GHC.Classes.== 200) (Network.HTTP.Client.Types.responseStatus response) ->
                                       GetUserByNameResponse200
                                         Data.Functor.<$> ( Data.Aeson.eitherDecodeStrict body ::
                                                              Data.Either.Either
                                                                GHC.Base.String
                                                                User
                                                          )
                                     | (\status_6 -> Network.HTTP.Types.Status.statusCode status_6 GHC.Classes.== 400) (Network.HTTP.Client.Types.responseStatus response) -> Data.Either.Right GetUserByNameResponse400
                                     | (\status_7 -> Network.HTTP.Types.Status.statusCode status_7 GHC.Classes.== 404) (Network.HTTP.Client.Types.responseStatus response) -> Data.Either.Right GetUserByNameResponse404
                                     | GHC.Base.otherwise -> Data.Either.Left "Missing default response type"
                             )
                    response_4
              )
              response_4
        )
    )
    (OpenAPI.Common.doCallWithConfigurationM (Data.Text.toUpper (Data.Text.pack "GET")) (Data.Text.pack ("/user/" GHC.Base.++ (Data.ByteString.Char8.unpack (Network.HTTP.Types.URI.urlEncode GHC.Types.True GHC.Base.$ (Data.ByteString.Char8.pack GHC.Base.$ OpenAPI.Common.stringifyModel username)) GHC.Base.++ ""))) [])

-- | > GET /user/{username}
--
-- Monadic version of 'getUserByNameRaw' (use with 'OpenAPI.Common.runWithConfiguration')
getUserByNameRawM ::
  forall m s.
  ( OpenAPI.Common.MonadHTTP m,
    OpenAPI.Common.SecurityScheme s
  ) =>
  GHC.Base.String ->
  Control.Monad.Trans.Reader.ReaderT
    (OpenAPI.Common.Configuration s)
    m
    ( Data.Either.Either
        Network.HTTP.Client.Types.HttpException
        (Network.HTTP.Client.Types.Response Data.ByteString.Internal.ByteString)
    )
getUserByNameRawM username = GHC.Base.id (OpenAPI.Common.doCallWithConfigurationM (Data.Text.toUpper (Data.Text.pack "GET")) (Data.Text.pack ("/user/" GHC.Base.++ (Data.ByteString.Char8.unpack (Network.HTTP.Types.URI.urlEncode GHC.Types.True GHC.Base.$ (Data.ByteString.Char8.pack GHC.Base.$ OpenAPI.Common.stringifyModel username)) GHC.Base.++ ""))) [])

-- | Represents a response of the operation 'getUserByName'.
--
-- The response constructor is chosen by the status code of the response. If no case matches (no specific case for the response code, no range case, no default case), 'GetUserByNameResponseError' is used.
data GetUserByNameResponse
  = -- | Means either no matching case available or a parse error
    GetUserByNameResponseError GHC.Base.String
  | -- | successful operation
    GetUserByNameResponse200 User
  | -- | Invalid username supplied
    GetUserByNameResponse400
  | -- | User not found
    GetUserByNameResponse404
  deriving (GHC.Show.Show, GHC.Classes.Eq)
