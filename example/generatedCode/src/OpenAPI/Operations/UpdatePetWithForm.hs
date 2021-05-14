{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE ExplicitForAll #-}
{-# LANGUAGE MultiWayIf #-}
{-# LANGUAGE OverloadedStrings #-}

-- | Contains the different functions to run the operation updatePetWithForm
module OpenAPI.Operations.UpdatePetWithForm where

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

-- | > POST /pet/{petId}
updatePetWithForm ::
  forall m s.
  (OpenAPI.Common.MonadHTTP m, OpenAPI.Common.SecurityScheme s) =>
  -- | The configuration to use in the request
  OpenAPI.Common.Configuration s ->
  -- | petId: ID of pet that needs to be updated
  GHC.Int.Int64 ->
  -- | The request body to send
  UpdatePetWithFormRequestBody ->
  -- | Monad containing the result of the operation
  m (Data.Either.Either Network.HTTP.Client.Types.HttpException (Network.HTTP.Client.Types.Response UpdatePetWithFormResponse))
updatePetWithForm
  config
  petId
  body =
    GHC.Base.fmap
      ( GHC.Base.fmap
          ( \response_0 ->
              GHC.Base.fmap
                ( Data.Either.either UpdatePetWithFormResponseError GHC.Base.id
                    GHC.Base.. ( \response body ->
                                   if
                                       | (\status_1 -> Network.HTTP.Types.Status.statusCode status_1 GHC.Classes.== 405) (Network.HTTP.Client.Types.responseStatus response) -> Data.Either.Right UpdatePetWithFormResponse405
                                       | GHC.Base.otherwise -> Data.Either.Left "Missing default response type"
                               )
                      response_0
                )
                response_0
          )
      )
      (OpenAPI.Common.doBodyCallWithConfiguration config (Data.Text.toUpper (Data.Text.pack "POST")) (Data.Text.pack ("/pet/" GHC.Base.++ (Data.ByteString.Char8.unpack (Network.HTTP.Types.URI.urlEncode GHC.Types.True GHC.Base.$ (Data.ByteString.Char8.pack GHC.Base.$ OpenAPI.Common.stringifyModel petId)) GHC.Base.++ ""))) [] body OpenAPI.Common.RequestBodyEncodingFormData)

-- | > POST /pet/{petId}
--
-- The same as 'updatePetWithForm' but returns the raw 'Data.ByteString.Char8.ByteString'
updatePetWithFormRaw ::
  forall m s.
  ( OpenAPI.Common.MonadHTTP m,
    OpenAPI.Common.SecurityScheme s
  ) =>
  OpenAPI.Common.Configuration s ->
  GHC.Int.Int64 ->
  UpdatePetWithFormRequestBody ->
  m
    ( Data.Either.Either
        Network.HTTP.Client.Types.HttpException
        (Network.HTTP.Client.Types.Response Data.ByteString.Internal.ByteString)
    )
updatePetWithFormRaw
  config
  petId
  body = GHC.Base.id (OpenAPI.Common.doBodyCallWithConfiguration config (Data.Text.toUpper (Data.Text.pack "POST")) (Data.Text.pack ("/pet/" GHC.Base.++ (Data.ByteString.Char8.unpack (Network.HTTP.Types.URI.urlEncode GHC.Types.True GHC.Base.$ (Data.ByteString.Char8.pack GHC.Base.$ OpenAPI.Common.stringifyModel petId)) GHC.Base.++ ""))) [] body OpenAPI.Common.RequestBodyEncodingFormData)

-- | > POST /pet/{petId}
--
-- Monadic version of 'updatePetWithForm' (use with 'OpenAPI.Common.runWithConfiguration')
updatePetWithFormM ::
  forall m s.
  ( OpenAPI.Common.MonadHTTP m,
    OpenAPI.Common.SecurityScheme s
  ) =>
  GHC.Int.Int64 ->
  UpdatePetWithFormRequestBody ->
  Control.Monad.Trans.Reader.ReaderT
    (OpenAPI.Common.Configuration s)
    m
    ( Data.Either.Either
        Network.HTTP.Client.Types.HttpException
        (Network.HTTP.Client.Types.Response UpdatePetWithFormResponse)
    )
updatePetWithFormM
  petId
  body =
    GHC.Base.fmap
      ( GHC.Base.fmap
          ( \response_2 ->
              GHC.Base.fmap
                ( Data.Either.either UpdatePetWithFormResponseError GHC.Base.id
                    GHC.Base.. ( \response body ->
                                   if
                                       | (\status_3 -> Network.HTTP.Types.Status.statusCode status_3 GHC.Classes.== 405) (Network.HTTP.Client.Types.responseStatus response) -> Data.Either.Right UpdatePetWithFormResponse405
                                       | GHC.Base.otherwise -> Data.Either.Left "Missing default response type"
                               )
                      response_2
                )
                response_2
          )
      )
      (OpenAPI.Common.doBodyCallWithConfigurationM (Data.Text.toUpper (Data.Text.pack "POST")) (Data.Text.pack ("/pet/" GHC.Base.++ (Data.ByteString.Char8.unpack (Network.HTTP.Types.URI.urlEncode GHC.Types.True GHC.Base.$ (Data.ByteString.Char8.pack GHC.Base.$ OpenAPI.Common.stringifyModel petId)) GHC.Base.++ ""))) [] body OpenAPI.Common.RequestBodyEncodingFormData)

-- | > POST /pet/{petId}
--
-- Monadic version of 'updatePetWithFormRaw' (use with 'OpenAPI.Common.runWithConfiguration')
updatePetWithFormRawM ::
  forall m s.
  ( OpenAPI.Common.MonadHTTP m,
    OpenAPI.Common.SecurityScheme s
  ) =>
  GHC.Int.Int64 ->
  UpdatePetWithFormRequestBody ->
  Control.Monad.Trans.Reader.ReaderT
    (OpenAPI.Common.Configuration s)
    m
    ( Data.Either.Either
        Network.HTTP.Client.Types.HttpException
        (Network.HTTP.Client.Types.Response Data.ByteString.Internal.ByteString)
    )
updatePetWithFormRawM
  petId
  body = GHC.Base.id (OpenAPI.Common.doBodyCallWithConfigurationM (Data.Text.toUpper (Data.Text.pack "POST")) (Data.Text.pack ("/pet/" GHC.Base.++ (Data.ByteString.Char8.unpack (Network.HTTP.Types.URI.urlEncode GHC.Types.True GHC.Base.$ (Data.ByteString.Char8.pack GHC.Base.$ OpenAPI.Common.stringifyModel petId)) GHC.Base.++ ""))) [] body OpenAPI.Common.RequestBodyEncodingFormData)

-- | Defines the data type for the schema updatePetWithFormRequestBody
data UpdatePetWithFormRequestBody = UpdatePetWithFormRequestBody
  { -- | name: Updated name of the pet
    updatePetWithFormRequestBodyName :: (GHC.Maybe.Maybe GHC.Base.String),
    -- | status: Updated status of the pet
    updatePetWithFormRequestBodyStatus :: (GHC.Maybe.Maybe GHC.Base.String)
  }
  deriving
    ( GHC.Show.Show,
      GHC.Classes.Eq
    )

instance Data.Aeson.ToJSON UpdatePetWithFormRequestBody where
  toJSON obj = Data.Aeson.object ((Data.Aeson..=) "name" (updatePetWithFormRequestBodyName obj) : (Data.Aeson..=) "status" (updatePetWithFormRequestBodyStatus obj) : [])
  toEncoding obj = Data.Aeson.pairs ((Data.Aeson..=) "name" (updatePetWithFormRequestBodyName obj) GHC.Base.<> (Data.Aeson..=) "status" (updatePetWithFormRequestBodyStatus obj))

instance Data.Aeson.Types.FromJSON.FromJSON UpdatePetWithFormRequestBody where
  parseJSON = Data.Aeson.Types.FromJSON.withObject "UpdatePetWithFormRequestBody" (\obj -> (GHC.Base.pure UpdatePetWithFormRequestBody GHC.Base.<*> (obj Data.Aeson.Types.FromJSON..:? "name")) GHC.Base.<*> (obj Data.Aeson.Types.FromJSON..:? "status"))

-- | Represents a response of the operation 'updatePetWithForm'.
--
-- The response constructor is chosen by the status code of the response. If no case matches (no specific case for the response code, no range case, no default case), 'UpdatePetWithFormResponseError' is used.
data UpdatePetWithFormResponse
  = -- | Means either no matching case available or a parse error
    UpdatePetWithFormResponseError GHC.Base.String
  | -- | Invalid input
    UpdatePetWithFormResponse405
  deriving (GHC.Show.Show, GHC.Classes.Eq)
