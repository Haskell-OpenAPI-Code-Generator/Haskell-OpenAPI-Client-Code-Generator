{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE ExplicitForAll #-}
{-# LANGUAGE MultiWayIf #-}
{-# LANGUAGE OverloadedStrings #-}

-- | Contains the different functions to run the operation updatePet
module OpenAPI.Operations.UpdatePet where

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

-- | > PUT /pet
updatePet ::
  forall m s.
  (OpenAPI.Common.MonadHTTP m, OpenAPI.Common.SecurityScheme s) =>
  -- | The configuration to use in the request
  OpenAPI.Common.Configuration s ->
  -- | The request body to send
  Pet ->
  -- | Monad containing the result of the operation
  m (Data.Either.Either Network.HTTP.Client.Types.HttpException (Network.HTTP.Client.Types.Response UpdatePetResponse))
updatePet
  config
  body =
    GHC.Base.fmap
      ( GHC.Base.fmap
          ( \response_0 ->
              GHC.Base.fmap
                ( Data.Either.either UpdatePetResponseError GHC.Base.id
                    GHC.Base.. ( \response body ->
                                   if
                                       | (\status_1 -> Network.HTTP.Types.Status.statusCode status_1 GHC.Classes.== 400) (Network.HTTP.Client.Types.responseStatus response) -> Data.Either.Right UpdatePetResponse400
                                       | (\status_2 -> Network.HTTP.Types.Status.statusCode status_2 GHC.Classes.== 404) (Network.HTTP.Client.Types.responseStatus response) -> Data.Either.Right UpdatePetResponse404
                                       | (\status_3 -> Network.HTTP.Types.Status.statusCode status_3 GHC.Classes.== 405) (Network.HTTP.Client.Types.responseStatus response) -> Data.Either.Right UpdatePetResponse405
                                       | GHC.Base.otherwise -> Data.Either.Left "Missing default response type"
                               )
                      response_0
                )
                response_0
          )
      )
      (OpenAPI.Common.doBodyCallWithConfiguration config (Data.Text.toUpper (Data.Text.pack "PUT")) (Data.Text.pack "/pet") [] body OpenAPI.Common.RequestBodyEncodingJSON)

-- | > PUT /pet
--
-- The same as 'updatePet' but returns the raw 'Data.ByteString.Char8.ByteString'
updatePetRaw ::
  forall m s.
  ( OpenAPI.Common.MonadHTTP m,
    OpenAPI.Common.SecurityScheme s
  ) =>
  OpenAPI.Common.Configuration s ->
  Pet ->
  m
    ( Data.Either.Either
        Network.HTTP.Client.Types.HttpException
        (Network.HTTP.Client.Types.Response Data.ByteString.Internal.ByteString)
    )
updatePetRaw
  config
  body = GHC.Base.id (OpenAPI.Common.doBodyCallWithConfiguration config (Data.Text.toUpper (Data.Text.pack "PUT")) (Data.Text.pack "/pet") [] body OpenAPI.Common.RequestBodyEncodingJSON)

-- | > PUT /pet
--
-- Monadic version of 'updatePet' (use with 'OpenAPI.Common.runWithConfiguration')
updatePetM ::
  forall m s.
  ( OpenAPI.Common.MonadHTTP m,
    OpenAPI.Common.SecurityScheme s
  ) =>
  Pet ->
  Control.Monad.Trans.Reader.ReaderT
    (OpenAPI.Common.Configuration s)
    m
    ( Data.Either.Either
        Network.HTTP.Client.Types.HttpException
        (Network.HTTP.Client.Types.Response UpdatePetResponse)
    )
updatePetM body =
  GHC.Base.fmap
    ( GHC.Base.fmap
        ( \response_4 ->
            GHC.Base.fmap
              ( Data.Either.either UpdatePetResponseError GHC.Base.id
                  GHC.Base.. ( \response body ->
                                 if
                                     | (\status_5 -> Network.HTTP.Types.Status.statusCode status_5 GHC.Classes.== 400) (Network.HTTP.Client.Types.responseStatus response) -> Data.Either.Right UpdatePetResponse400
                                     | (\status_6 -> Network.HTTP.Types.Status.statusCode status_6 GHC.Classes.== 404) (Network.HTTP.Client.Types.responseStatus response) -> Data.Either.Right UpdatePetResponse404
                                     | (\status_7 -> Network.HTTP.Types.Status.statusCode status_7 GHC.Classes.== 405) (Network.HTTP.Client.Types.responseStatus response) -> Data.Either.Right UpdatePetResponse405
                                     | GHC.Base.otherwise -> Data.Either.Left "Missing default response type"
                             )
                    response_4
              )
              response_4
        )
    )
    (OpenAPI.Common.doBodyCallWithConfigurationM (Data.Text.toUpper (Data.Text.pack "PUT")) (Data.Text.pack "/pet") [] body OpenAPI.Common.RequestBodyEncodingJSON)

-- | > PUT /pet
--
-- Monadic version of 'updatePetRaw' (use with 'OpenAPI.Common.runWithConfiguration')
updatePetRawM ::
  forall m s.
  ( OpenAPI.Common.MonadHTTP m,
    OpenAPI.Common.SecurityScheme s
  ) =>
  Pet ->
  Control.Monad.Trans.Reader.ReaderT
    (OpenAPI.Common.Configuration s)
    m
    ( Data.Either.Either
        Network.HTTP.Client.Types.HttpException
        (Network.HTTP.Client.Types.Response Data.ByteString.Internal.ByteString)
    )
updatePetRawM body = GHC.Base.id (OpenAPI.Common.doBodyCallWithConfigurationM (Data.Text.toUpper (Data.Text.pack "PUT")) (Data.Text.pack "/pet") [] body OpenAPI.Common.RequestBodyEncodingJSON)

-- | Represents a response of the operation 'updatePet'.
--
-- The response constructor is chosen by the status code of the response. If no case matches (no specific case for the response code, no range case, no default case), 'UpdatePetResponseError' is used.
data UpdatePetResponse
  = -- | Means either no matching case available or a parse error
    UpdatePetResponseError GHC.Base.String
  | -- | Invalid ID supplied
    UpdatePetResponse400
  | -- | Pet not found
    UpdatePetResponse404
  | -- | Validation exception
    UpdatePetResponse405
  deriving (GHC.Show.Show, GHC.Classes.Eq)
