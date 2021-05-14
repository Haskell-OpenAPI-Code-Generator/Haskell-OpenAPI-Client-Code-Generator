{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE ExplicitForAll #-}
{-# LANGUAGE MultiWayIf #-}
{-# LANGUAGE OverloadedStrings #-}

-- | Contains the different functions to run the operation uploadFile
module OpenAPI.Operations.UploadFile where

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

-- | > POST /pet/{petId}/uploadImage
uploadFile ::
  forall m s.
  (OpenAPI.Common.MonadHTTP m, OpenAPI.Common.SecurityScheme s) =>
  -- | The configuration to use in the request
  OpenAPI.Common.Configuration s ->
  -- | petId: ID of pet to update
  GHC.Int.Int64 ->
  -- | Monad containing the result of the operation
  m (Data.Either.Either Network.HTTP.Client.Types.HttpException (Network.HTTP.Client.Types.Response UploadFileResponse))
uploadFile
  config
  petId =
    GHC.Base.fmap
      ( GHC.Base.fmap
          ( \response_0 ->
              GHC.Base.fmap
                ( Data.Either.either UploadFileResponseError GHC.Base.id
                    GHC.Base.. ( \response body ->
                                   if
                                       | (\status_1 -> Network.HTTP.Types.Status.statusCode status_1 GHC.Classes.== 200) (Network.HTTP.Client.Types.responseStatus response) ->
                                         UploadFileResponse200
                                           Data.Functor.<$> ( Data.Aeson.eitherDecodeStrict body ::
                                                                Data.Either.Either
                                                                  GHC.Base.String
                                                                  ApiResponse
                                                            )
                                       | GHC.Base.otherwise -> Data.Either.Left "Missing default response type"
                               )
                      response_0
                )
                response_0
          )
      )
      (OpenAPI.Common.doCallWithConfiguration config (Data.Text.toUpper (Data.Text.pack "POST")) (Data.Text.pack ("/pet/" GHC.Base.++ (Data.ByteString.Char8.unpack (Network.HTTP.Types.URI.urlEncode GHC.Types.True GHC.Base.$ (Data.ByteString.Char8.pack GHC.Base.$ OpenAPI.Common.stringifyModel petId)) GHC.Base.++ "/uploadImage"))) [])

-- | > POST /pet/{petId}/uploadImage
--
-- The same as 'uploadFile' but returns the raw 'Data.ByteString.Char8.ByteString'
uploadFileRaw ::
  forall m s.
  ( OpenAPI.Common.MonadHTTP m,
    OpenAPI.Common.SecurityScheme s
  ) =>
  OpenAPI.Common.Configuration s ->
  GHC.Int.Int64 ->
  m
    ( Data.Either.Either
        Network.HTTP.Client.Types.HttpException
        (Network.HTTP.Client.Types.Response Data.ByteString.Internal.ByteString)
    )
uploadFileRaw
  config
  petId = GHC.Base.id (OpenAPI.Common.doCallWithConfiguration config (Data.Text.toUpper (Data.Text.pack "POST")) (Data.Text.pack ("/pet/" GHC.Base.++ (Data.ByteString.Char8.unpack (Network.HTTP.Types.URI.urlEncode GHC.Types.True GHC.Base.$ (Data.ByteString.Char8.pack GHC.Base.$ OpenAPI.Common.stringifyModel petId)) GHC.Base.++ "/uploadImage"))) [])

-- | > POST /pet/{petId}/uploadImage
--
-- Monadic version of 'uploadFile' (use with 'OpenAPI.Common.runWithConfiguration')
uploadFileM ::
  forall m s.
  ( OpenAPI.Common.MonadHTTP m,
    OpenAPI.Common.SecurityScheme s
  ) =>
  GHC.Int.Int64 ->
  Control.Monad.Trans.Reader.ReaderT
    (OpenAPI.Common.Configuration s)
    m
    ( Data.Either.Either
        Network.HTTP.Client.Types.HttpException
        (Network.HTTP.Client.Types.Response UploadFileResponse)
    )
uploadFileM petId =
  GHC.Base.fmap
    ( GHC.Base.fmap
        ( \response_2 ->
            GHC.Base.fmap
              ( Data.Either.either UploadFileResponseError GHC.Base.id
                  GHC.Base.. ( \response body ->
                                 if
                                     | (\status_3 -> Network.HTTP.Types.Status.statusCode status_3 GHC.Classes.== 200) (Network.HTTP.Client.Types.responseStatus response) ->
                                       UploadFileResponse200
                                         Data.Functor.<$> ( Data.Aeson.eitherDecodeStrict body ::
                                                              Data.Either.Either
                                                                GHC.Base.String
                                                                ApiResponse
                                                          )
                                     | GHC.Base.otherwise -> Data.Either.Left "Missing default response type"
                             )
                    response_2
              )
              response_2
        )
    )
    (OpenAPI.Common.doCallWithConfigurationM (Data.Text.toUpper (Data.Text.pack "POST")) (Data.Text.pack ("/pet/" GHC.Base.++ (Data.ByteString.Char8.unpack (Network.HTTP.Types.URI.urlEncode GHC.Types.True GHC.Base.$ (Data.ByteString.Char8.pack GHC.Base.$ OpenAPI.Common.stringifyModel petId)) GHC.Base.++ "/uploadImage"))) [])

-- | > POST /pet/{petId}/uploadImage
--
-- Monadic version of 'uploadFileRaw' (use with 'OpenAPI.Common.runWithConfiguration')
uploadFileRawM ::
  forall m s.
  ( OpenAPI.Common.MonadHTTP m,
    OpenAPI.Common.SecurityScheme s
  ) =>
  GHC.Int.Int64 ->
  Control.Monad.Trans.Reader.ReaderT
    (OpenAPI.Common.Configuration s)
    m
    ( Data.Either.Either
        Network.HTTP.Client.Types.HttpException
        (Network.HTTP.Client.Types.Response Data.ByteString.Internal.ByteString)
    )
uploadFileRawM petId = GHC.Base.id (OpenAPI.Common.doCallWithConfigurationM (Data.Text.toUpper (Data.Text.pack "POST")) (Data.Text.pack ("/pet/" GHC.Base.++ (Data.ByteString.Char8.unpack (Network.HTTP.Types.URI.urlEncode GHC.Types.True GHC.Base.$ (Data.ByteString.Char8.pack GHC.Base.$ OpenAPI.Common.stringifyModel petId)) GHC.Base.++ "/uploadImage"))) [])

-- | Represents a response of the operation 'uploadFile'.
--
-- The response constructor is chosen by the status code of the response. If no case matches (no specific case for the response code, no range case, no default case), 'UploadFileResponseError' is used.
data UploadFileResponse
  = -- | Means either no matching case available or a parse error
    UploadFileResponseError GHC.Base.String
  | -- | successful operation
    UploadFileResponse200 ApiResponse
  deriving (GHC.Show.Show, GHC.Classes.Eq)
