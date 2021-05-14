{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE ExplicitForAll #-}
{-# LANGUAGE MultiWayIf #-}
{-# LANGUAGE OverloadedStrings #-}

-- | Contains the different functions to run the operation loginUser
module OpenAPI.Operations.LoginUser where

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

-- | > GET /user/login
loginUser ::
  forall m s.
  (OpenAPI.Common.MonadHTTP m, OpenAPI.Common.SecurityScheme s) =>
  -- | The configuration to use in the request
  OpenAPI.Common.Configuration s ->
  -- | username: The user name for login
  GHC.Base.String ->
  -- | password: The password for login in clear text
  GHC.Base.String ->
  -- | Monad containing the result of the operation
  m (Data.Either.Either Network.HTTP.Client.Types.HttpException (Network.HTTP.Client.Types.Response LoginUserResponse))
loginUser
  config
  username
  password =
    GHC.Base.fmap
      ( GHC.Base.fmap
          ( \response_0 ->
              GHC.Base.fmap
                ( Data.Either.either LoginUserResponseError GHC.Base.id
                    GHC.Base.. ( \response body ->
                                   if
                                       | (\status_1 -> Network.HTTP.Types.Status.statusCode status_1 GHC.Classes.== 200) (Network.HTTP.Client.Types.responseStatus response) ->
                                         LoginUserResponse200
                                           Data.Functor.<$> ( Data.Aeson.eitherDecodeStrict body ::
                                                                Data.Either.Either
                                                                  GHC.Base.String
                                                                  GHC.Base.String
                                                            )
                                       | (\status_2 -> Network.HTTP.Types.Status.statusCode status_2 GHC.Classes.== 400) (Network.HTTP.Client.Types.responseStatus response) -> Data.Either.Right LoginUserResponse400
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
          (Data.Text.pack "/user/login")
          ( ( Data.Text.pack "username",
              GHC.Base.Just GHC.Base.$ OpenAPI.Common.stringifyModel username
            ) :
            ( ( Data.Text.pack "password",
                GHC.Base.Just GHC.Base.$ OpenAPI.Common.stringifyModel password
              ) :
              []
            )
          )
      )

-- | > GET /user/login
--
-- The same as 'loginUser' but returns the raw 'Data.ByteString.Char8.ByteString'
loginUserRaw ::
  forall m s.
  ( OpenAPI.Common.MonadHTTP m,
    OpenAPI.Common.SecurityScheme s
  ) =>
  OpenAPI.Common.Configuration s ->
  GHC.Base.String ->
  GHC.Base.String ->
  m
    ( Data.Either.Either
        Network.HTTP.Client.Types.HttpException
        (Network.HTTP.Client.Types.Response Data.ByteString.Internal.ByteString)
    )
loginUserRaw
  config
  username
  password =
    GHC.Base.id
      ( OpenAPI.Common.doCallWithConfiguration
          config
          (Data.Text.toUpper (Data.Text.pack "GET"))
          (Data.Text.pack "/user/login")
          ( ( Data.Text.pack "username",
              GHC.Base.Just GHC.Base.$ OpenAPI.Common.stringifyModel username
            ) :
            ( ( Data.Text.pack "password",
                GHC.Base.Just GHC.Base.$ OpenAPI.Common.stringifyModel password
              ) :
              []
            )
          )
      )

-- | > GET /user/login
--
-- Monadic version of 'loginUser' (use with 'OpenAPI.Common.runWithConfiguration')
loginUserM ::
  forall m s.
  ( OpenAPI.Common.MonadHTTP m,
    OpenAPI.Common.SecurityScheme s
  ) =>
  GHC.Base.String ->
  GHC.Base.String ->
  Control.Monad.Trans.Reader.ReaderT
    (OpenAPI.Common.Configuration s)
    m
    ( Data.Either.Either
        Network.HTTP.Client.Types.HttpException
        (Network.HTTP.Client.Types.Response LoginUserResponse)
    )
loginUserM
  username
  password =
    GHC.Base.fmap
      ( GHC.Base.fmap
          ( \response_3 ->
              GHC.Base.fmap
                ( Data.Either.either LoginUserResponseError GHC.Base.id
                    GHC.Base.. ( \response body ->
                                   if
                                       | (\status_4 -> Network.HTTP.Types.Status.statusCode status_4 GHC.Classes.== 200) (Network.HTTP.Client.Types.responseStatus response) ->
                                         LoginUserResponse200
                                           Data.Functor.<$> ( Data.Aeson.eitherDecodeStrict body ::
                                                                Data.Either.Either
                                                                  GHC.Base.String
                                                                  GHC.Base.String
                                                            )
                                       | (\status_5 -> Network.HTTP.Types.Status.statusCode status_5 GHC.Classes.== 400) (Network.HTTP.Client.Types.responseStatus response) -> Data.Either.Right LoginUserResponse400
                                       | GHC.Base.otherwise -> Data.Either.Left "Missing default response type"
                               )
                      response_3
                )
                response_3
          )
      )
      ( OpenAPI.Common.doCallWithConfigurationM
          (Data.Text.toUpper (Data.Text.pack "GET"))
          (Data.Text.pack "/user/login")
          ( ( Data.Text.pack "username",
              GHC.Base.Just GHC.Base.$ OpenAPI.Common.stringifyModel username
            ) :
            ( ( Data.Text.pack "password",
                GHC.Base.Just GHC.Base.$ OpenAPI.Common.stringifyModel password
              ) :
              []
            )
          )
      )

-- | > GET /user/login
--
-- Monadic version of 'loginUserRaw' (use with 'OpenAPI.Common.runWithConfiguration')
loginUserRawM ::
  forall m s.
  ( OpenAPI.Common.MonadHTTP m,
    OpenAPI.Common.SecurityScheme s
  ) =>
  GHC.Base.String ->
  GHC.Base.String ->
  Control.Monad.Trans.Reader.ReaderT
    (OpenAPI.Common.Configuration s)
    m
    ( Data.Either.Either
        Network.HTTP.Client.Types.HttpException
        (Network.HTTP.Client.Types.Response Data.ByteString.Internal.ByteString)
    )
loginUserRawM
  username
  password =
    GHC.Base.id
      ( OpenAPI.Common.doCallWithConfigurationM
          (Data.Text.toUpper (Data.Text.pack "GET"))
          (Data.Text.pack "/user/login")
          ( ( Data.Text.pack "username",
              GHC.Base.Just GHC.Base.$ OpenAPI.Common.stringifyModel username
            ) :
            ( ( Data.Text.pack "password",
                GHC.Base.Just GHC.Base.$ OpenAPI.Common.stringifyModel password
              ) :
              []
            )
          )
      )

-- | Represents a response of the operation 'loginUser'.
--
-- The response constructor is chosen by the status code of the response. If no case matches (no specific case for the response code, no range case, no default case), 'LoginUserResponseError' is used.
data LoginUserResponse
  = -- | Means either no matching case available or a parse error
    LoginUserResponseError GHC.Base.String
  | -- | successful operation
    LoginUserResponse200 GHC.Base.String
  | -- | Invalid username\/password supplied
    LoginUserResponse400
  deriving (GHC.Show.Show, GHC.Classes.Eq)
