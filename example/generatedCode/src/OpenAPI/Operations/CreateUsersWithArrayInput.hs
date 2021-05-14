{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE ExplicitForAll #-}
{-# LANGUAGE MultiWayIf #-}
{-# LANGUAGE OverloadedStrings #-}

-- | Contains the different functions to run the operation createUsersWithArrayInput
module OpenAPI.Operations.CreateUsersWithArrayInput where

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

-- | > POST /user/createWithArray
createUsersWithArrayInput ::
  forall m s.
  (OpenAPI.Common.MonadHTTP m, OpenAPI.Common.SecurityScheme s) =>
  -- | The configuration to use in the request
  OpenAPI.Common.Configuration s ->
  -- | The request body to send
  [] User ->
  -- | Monad containing the result of the operation
  m (Data.Either.Either Network.HTTP.Client.Types.HttpException (Network.HTTP.Client.Types.Response CreateUsersWithArrayInputResponse))
createUsersWithArrayInput
  config
  body =
    GHC.Base.fmap
      ( GHC.Base.fmap
          ( \response_0 ->
              GHC.Base.fmap
                ( Data.Either.either CreateUsersWithArrayInputResponseError GHC.Base.id
                    GHC.Base.. ( \response body ->
                                   if
                                       | GHC.Base.const GHC.Types.True (Network.HTTP.Client.Types.responseStatus response) -> Data.Either.Right CreateUsersWithArrayInputResponseDefault
                                       | GHC.Base.otherwise -> Data.Either.Left "Missing default response type"
                               )
                      response_0
                )
                response_0
          )
      )
      (OpenAPI.Common.doBodyCallWithConfiguration config (Data.Text.toUpper (Data.Text.pack "POST")) (Data.Text.pack "/user/createWithArray") [] body OpenAPI.Common.RequestBodyEncodingJSON)

-- | > POST /user/createWithArray
--
-- The same as 'createUsersWithArrayInput' but returns the raw 'Data.ByteString.Char8.ByteString'
createUsersWithArrayInputRaw ::
  forall m s.
  ( OpenAPI.Common.MonadHTTP m,
    OpenAPI.Common.SecurityScheme s
  ) =>
  OpenAPI.Common.Configuration s ->
  [] User ->
  m
    ( Data.Either.Either
        Network.HTTP.Client.Types.HttpException
        (Network.HTTP.Client.Types.Response Data.ByteString.Internal.ByteString)
    )
createUsersWithArrayInputRaw
  config
  body = GHC.Base.id (OpenAPI.Common.doBodyCallWithConfiguration config (Data.Text.toUpper (Data.Text.pack "POST")) (Data.Text.pack "/user/createWithArray") [] body OpenAPI.Common.RequestBodyEncodingJSON)

-- | > POST /user/createWithArray
--
-- Monadic version of 'createUsersWithArrayInput' (use with 'OpenAPI.Common.runWithConfiguration')
createUsersWithArrayInputM ::
  forall m s.
  ( OpenAPI.Common.MonadHTTP m,
    OpenAPI.Common.SecurityScheme s
  ) =>
  [] User ->
  Control.Monad.Trans.Reader.ReaderT
    (OpenAPI.Common.Configuration s)
    m
    ( Data.Either.Either
        Network.HTTP.Client.Types.HttpException
        (Network.HTTP.Client.Types.Response CreateUsersWithArrayInputResponse)
    )
createUsersWithArrayInputM body =
  GHC.Base.fmap
    ( GHC.Base.fmap
        ( \response_1 ->
            GHC.Base.fmap
              ( Data.Either.either CreateUsersWithArrayInputResponseError GHC.Base.id
                  GHC.Base.. ( \response body ->
                                 if
                                     | GHC.Base.const GHC.Types.True (Network.HTTP.Client.Types.responseStatus response) -> Data.Either.Right CreateUsersWithArrayInputResponseDefault
                                     | GHC.Base.otherwise -> Data.Either.Left "Missing default response type"
                             )
                    response_1
              )
              response_1
        )
    )
    (OpenAPI.Common.doBodyCallWithConfigurationM (Data.Text.toUpper (Data.Text.pack "POST")) (Data.Text.pack "/user/createWithArray") [] body OpenAPI.Common.RequestBodyEncodingJSON)

-- | > POST /user/createWithArray
--
-- Monadic version of 'createUsersWithArrayInputRaw' (use with 'OpenAPI.Common.runWithConfiguration')
createUsersWithArrayInputRawM ::
  forall m s.
  ( OpenAPI.Common.MonadHTTP m,
    OpenAPI.Common.SecurityScheme s
  ) =>
  [] User ->
  Control.Monad.Trans.Reader.ReaderT
    (OpenAPI.Common.Configuration s)
    m
    ( Data.Either.Either
        Network.HTTP.Client.Types.HttpException
        (Network.HTTP.Client.Types.Response Data.ByteString.Internal.ByteString)
    )
createUsersWithArrayInputRawM body = GHC.Base.id (OpenAPI.Common.doBodyCallWithConfigurationM (Data.Text.toUpper (Data.Text.pack "POST")) (Data.Text.pack "/user/createWithArray") [] body OpenAPI.Common.RequestBodyEncodingJSON)

-- | Represents a response of the operation 'createUsersWithArrayInput'.
--
-- The response constructor is chosen by the status code of the response. If no case matches (no specific case for the response code, no range case, no default case), 'CreateUsersWithArrayInputResponseError' is used.
data CreateUsersWithArrayInputResponse
  = -- | Means either no matching case available or a parse error
    CreateUsersWithArrayInputResponseError GHC.Base.String
  | -- | successful operation
    CreateUsersWithArrayInputResponseDefault
  deriving (GHC.Show.Show, GHC.Classes.Eq)
