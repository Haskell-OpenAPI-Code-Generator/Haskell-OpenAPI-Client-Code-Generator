{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE ExplicitForAll #-}
{-# LANGUAGE MultiWayIf #-}
{-# LANGUAGE OverloadedStrings #-}

-- | Contains the different functions to run the operation findPetsByStatus
module OpenAPI.Operations.FindPetsByStatus where

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

-- | > GET /pet/findByStatus
--
-- Multiple status values can be provided with comma separated strings
findPetsByStatus ::
  forall m s.
  (OpenAPI.Common.MonadHTTP m, OpenAPI.Common.SecurityScheme s) =>
  -- | The configuration to use in the request
  OpenAPI.Common.Configuration s ->
  -- | status: Status values that need to be considered for filter
  GHC.Base.String ->
  -- | Monad containing the result of the operation
  m (Data.Either.Either Network.HTTP.Client.Types.HttpException (Network.HTTP.Client.Types.Response FindPetsByStatusResponse))
findPetsByStatus
  config
  status =
    GHC.Base.fmap
      ( GHC.Base.fmap
          ( \response_0 ->
              GHC.Base.fmap
                ( Data.Either.either FindPetsByStatusResponseError GHC.Base.id
                    GHC.Base.. ( \response body ->
                                   if
                                       | (\status_1 -> Network.HTTP.Types.Status.statusCode status_1 GHC.Classes.== 200) (Network.HTTP.Client.Types.responseStatus response) ->
                                         FindPetsByStatusResponse200
                                           Data.Functor.<$> ( Data.Aeson.eitherDecodeStrict body ::
                                                                Data.Either.Either
                                                                  GHC.Base.String
                                                                  ([] Pet)
                                                            )
                                       | (\status_2 -> Network.HTTP.Types.Status.statusCode status_2 GHC.Classes.== 400) (Network.HTTP.Client.Types.responseStatus response) -> Data.Either.Right FindPetsByStatusResponse400
                                       | GHC.Base.otherwise -> Data.Either.Left "Missing default response type"
                               )
                      response_0
                )
                response_0
          )
      )
      ( OpenAPI.Common.doCallWithConfiguration
          config
          (Data.Text.toUpper (Data.Text.pack "GET"))
          (Data.Text.pack "/pet/findByStatus")
          ( ( Data.Text.pack "status",
              GHC.Base.Just GHC.Base.$ OpenAPI.Common.stringifyModel status
            ) :
            []
          )
      )

-- | > GET /pet/findByStatus
--
-- The same as 'findPetsByStatus' but returns the raw 'Data.ByteString.Char8.ByteString'
findPetsByStatusRaw ::
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
findPetsByStatusRaw
  config
  status =
    GHC.Base.id
      ( OpenAPI.Common.doCallWithConfiguration
          config
          (Data.Text.toUpper (Data.Text.pack "GET"))
          (Data.Text.pack "/pet/findByStatus")
          ( ( Data.Text.pack "status",
              GHC.Base.Just GHC.Base.$ OpenAPI.Common.stringifyModel status
            ) :
            []
          )
      )

-- | > GET /pet/findByStatus
--
-- Monadic version of 'findPetsByStatus' (use with 'OpenAPI.Common.runWithConfiguration')
findPetsByStatusM ::
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
        (Network.HTTP.Client.Types.Response FindPetsByStatusResponse)
    )
findPetsByStatusM status =
  GHC.Base.fmap
    ( GHC.Base.fmap
        ( \response_3 ->
            GHC.Base.fmap
              ( Data.Either.either FindPetsByStatusResponseError GHC.Base.id
                  GHC.Base.. ( \response body ->
                                 if
                                     | (\status_4 -> Network.HTTP.Types.Status.statusCode status_4 GHC.Classes.== 200) (Network.HTTP.Client.Types.responseStatus response) ->
                                       FindPetsByStatusResponse200
                                         Data.Functor.<$> ( Data.Aeson.eitherDecodeStrict body ::
                                                              Data.Either.Either
                                                                GHC.Base.String
                                                                ([] Pet)
                                                          )
                                     | (\status_5 -> Network.HTTP.Types.Status.statusCode status_5 GHC.Classes.== 400) (Network.HTTP.Client.Types.responseStatus response) -> Data.Either.Right FindPetsByStatusResponse400
                                     | GHC.Base.otherwise -> Data.Either.Left "Missing default response type"
                             )
                    response_3
              )
              response_3
        )
    )
    ( OpenAPI.Common.doCallWithConfigurationM
        (Data.Text.toUpper (Data.Text.pack "GET"))
        (Data.Text.pack "/pet/findByStatus")
        ( ( Data.Text.pack "status",
            GHC.Base.Just GHC.Base.$ OpenAPI.Common.stringifyModel status
          ) :
          []
        )
    )

-- | > GET /pet/findByStatus
--
-- Monadic version of 'findPetsByStatusRaw' (use with 'OpenAPI.Common.runWithConfiguration')
findPetsByStatusRawM ::
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
findPetsByStatusRawM status =
  GHC.Base.id
    ( OpenAPI.Common.doCallWithConfigurationM
        (Data.Text.toUpper (Data.Text.pack "GET"))
        (Data.Text.pack "/pet/findByStatus")
        ( ( Data.Text.pack "status",
            GHC.Base.Just GHC.Base.$ OpenAPI.Common.stringifyModel status
          ) :
          []
        )
    )

-- | Represents a response of the operation 'findPetsByStatus'.
--
-- The response constructor is chosen by the status code of the response. If no case matches (no specific case for the response code, no range case, no default case), 'FindPetsByStatusResponseError' is used.
data FindPetsByStatusResponse
  = -- | Means either no matching case available or a parse error
    FindPetsByStatusResponseError GHC.Base.String
  | -- | successful operation
    FindPetsByStatusResponse200 ([] Pet)
  | -- | Invalid status value
    FindPetsByStatusResponse400
  deriving (GHC.Show.Show, GHC.Classes.Eq)
