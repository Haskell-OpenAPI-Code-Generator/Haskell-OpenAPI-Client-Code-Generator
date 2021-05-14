{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE ExplicitForAll #-}
{-# LANGUAGE MultiWayIf #-}
{-# LANGUAGE OverloadedStrings #-}

-- | Contains the different functions to run the operation logoutUser
module OpenAPI.Operations.LogoutUser where

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

-- | > GET /user/logout
logoutUser ::
  forall m s.
  (OpenAPI.Common.MonadHTTP m, OpenAPI.Common.SecurityScheme s) =>
  -- | The configuration to use in the request
  OpenAPI.Common.Configuration s ->
  -- | Monad containing the result of the operation
  m (Data.Either.Either Network.HTTP.Client.Types.HttpException (Network.HTTP.Client.Types.Response LogoutUserResponse))
logoutUser config =
  GHC.Base.fmap
    ( GHC.Base.fmap
        ( \response_0 ->
            GHC.Base.fmap
              ( Data.Either.either LogoutUserResponseError GHC.Base.id
                  GHC.Base.. ( \response body ->
                                 if
                                     | GHC.Base.const GHC.Types.True (Network.HTTP.Client.Types.responseStatus response) -> Data.Either.Right LogoutUserResponseDefault
                                     | GHC.Base.otherwise -> Data.Either.Left "Missing default response type"
                             )
                    response_0
              )
              response_0
        )
    )
    (OpenAPI.Common.doCallWithConfiguration config (Data.Text.toUpper (Data.Text.pack "GET")) (Data.Text.pack "/user/logout") [])

-- | > GET /user/logout
--
-- The same as 'logoutUser' but returns the raw 'Data.ByteString.Char8.ByteString'
logoutUserRaw ::
  forall m s.
  ( OpenAPI.Common.MonadHTTP m,
    OpenAPI.Common.SecurityScheme s
  ) =>
  OpenAPI.Common.Configuration s ->
  m
    ( Data.Either.Either
        Network.HTTP.Client.Types.HttpException
        (Network.HTTP.Client.Types.Response Data.ByteString.Internal.ByteString)
    )
logoutUserRaw config = GHC.Base.id (OpenAPI.Common.doCallWithConfiguration config (Data.Text.toUpper (Data.Text.pack "GET")) (Data.Text.pack "/user/logout") [])

-- | > GET /user/logout
--
-- Monadic version of 'logoutUser' (use with 'OpenAPI.Common.runWithConfiguration')
logoutUserM ::
  forall m s.
  ( OpenAPI.Common.MonadHTTP m,
    OpenAPI.Common.SecurityScheme s
  ) =>
  Control.Monad.Trans.Reader.ReaderT
    (OpenAPI.Common.Configuration s)
    m
    ( Data.Either.Either
        Network.HTTP.Client.Types.HttpException
        (Network.HTTP.Client.Types.Response LogoutUserResponse)
    )
logoutUserM =
  GHC.Base.fmap
    ( GHC.Base.fmap
        ( \response_1 ->
            GHC.Base.fmap
              ( Data.Either.either LogoutUserResponseError GHC.Base.id
                  GHC.Base.. ( \response body ->
                                 if
                                     | GHC.Base.const GHC.Types.True (Network.HTTP.Client.Types.responseStatus response) -> Data.Either.Right LogoutUserResponseDefault
                                     | GHC.Base.otherwise -> Data.Either.Left "Missing default response type"
                             )
                    response_1
              )
              response_1
        )
    )
    (OpenAPI.Common.doCallWithConfigurationM (Data.Text.toUpper (Data.Text.pack "GET")) (Data.Text.pack "/user/logout") [])

-- | > GET /user/logout
--
-- Monadic version of 'logoutUserRaw' (use with 'OpenAPI.Common.runWithConfiguration')
logoutUserRawM ::
  forall m s.
  ( OpenAPI.Common.MonadHTTP m,
    OpenAPI.Common.SecurityScheme s
  ) =>
  Control.Monad.Trans.Reader.ReaderT
    (OpenAPI.Common.Configuration s)
    m
    ( Data.Either.Either
        Network.HTTP.Client.Types.HttpException
        (Network.HTTP.Client.Types.Response Data.ByteString.Internal.ByteString)
    )
logoutUserRawM = GHC.Base.id (OpenAPI.Common.doCallWithConfigurationM (Data.Text.toUpper (Data.Text.pack "GET")) (Data.Text.pack "/user/logout") [])

-- | Represents a response of the operation 'logoutUser'.
--
-- The response constructor is chosen by the status code of the response. If no case matches (no specific case for the response code, no range case, no default case), 'LogoutUserResponseError' is used.
data LogoutUserResponse
  = -- | Means either no matching case available or a parse error
    LogoutUserResponseError GHC.Base.String
  | -- | successful operation
    LogoutUserResponseDefault
  deriving (GHC.Show.Show, GHC.Classes.Eq)
