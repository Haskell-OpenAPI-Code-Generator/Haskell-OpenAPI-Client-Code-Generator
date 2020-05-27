{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ExplicitForAll #-}
{-# LANGUAGE MultiWayIf #-}
{-# LANGUAGE DeriveGeneric #-}

-- | Contains the different functions to run the operation createUsersWithListInput
module OpenAPI.Operations.CreateUsersWithListInput where

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

-- | > POST /user/createWithList
-- 
-- 
createUsersWithListInput :: forall m s . (OpenAPI.Common.MonadHTTP m, OpenAPI.Common.SecurityScheme s) => OpenAPI.Common.Configuration s  -- ^ The configuration to use in the request
  -> [] User                                                                                                                              -- ^ The request body to send
  -> m (Data.Either.Either Network.HTTP.Client.Types.HttpException (Network.HTTP.Client.Types.Response CreateUsersWithListInputResponse)) -- ^ Monad containing the result of the operation
createUsersWithListInput config
                         body = GHC.Base.fmap (GHC.Base.fmap (\response_0 -> GHC.Base.fmap (Data.Either.either CreateUsersWithListInputResponseError GHC.Base.id GHC.Base.. (\response body -> if | GHC.Base.const GHC.Types.True (Network.HTTP.Client.Types.responseStatus response) -> Data.Either.Right CreateUsersWithListInputResponseDefault
                                                                                                                                                                                                  | GHC.Base.otherwise -> Data.Either.Left "Missing default response type") response_0) response_0)) (OpenAPI.Common.doBodyCallWithConfiguration config (Data.Text.toUpper (Data.Text.pack "POST")) (Data.Text.pack "/user/createWithList") [] body OpenAPI.Common.RequestBodyEncodingJSON)
-- | > POST /user/createWithList
-- 
-- The same as 'createUsersWithListInput' but returns the raw 'Data.ByteString.Char8.ByteString'
createUsersWithListInputRaw :: forall m s . (OpenAPI.Common.MonadHTTP m,
                                             OpenAPI.Common.SecurityScheme s) =>
                               OpenAPI.Common.Configuration s ->
                               [] User ->
                               m (Data.Either.Either Network.HTTP.Client.Types.HttpException
                                                     (Network.HTTP.Client.Types.Response Data.ByteString.Internal.ByteString))
createUsersWithListInputRaw config
                            body = GHC.Base.id (OpenAPI.Common.doBodyCallWithConfiguration config (Data.Text.toUpper (Data.Text.pack "POST")) (Data.Text.pack "/user/createWithList") [] body OpenAPI.Common.RequestBodyEncodingJSON)
-- | > POST /user/createWithList
-- 
-- Monadic version of 'createUsersWithListInput' (use with 'OpenAPI.Common.runWithConfiguration')
createUsersWithListInputM :: forall m s . (OpenAPI.Common.MonadHTTP m,
                                           OpenAPI.Common.SecurityScheme s) =>
                             [] User ->
                             Control.Monad.Trans.Reader.ReaderT (OpenAPI.Common.Configuration s)
                                                                m
                                                                (Data.Either.Either Network.HTTP.Client.Types.HttpException
                                                                                    (Network.HTTP.Client.Types.Response CreateUsersWithListInputResponse))
createUsersWithListInputM body = GHC.Base.fmap (GHC.Base.fmap (\response_1 -> GHC.Base.fmap (Data.Either.either CreateUsersWithListInputResponseError GHC.Base.id GHC.Base.. (\response body -> if | GHC.Base.const GHC.Types.True (Network.HTTP.Client.Types.responseStatus response) -> Data.Either.Right CreateUsersWithListInputResponseDefault
                                                                                                                                                                                                   | GHC.Base.otherwise -> Data.Either.Left "Missing default response type") response_1) response_1)) (OpenAPI.Common.doBodyCallWithConfigurationM (Data.Text.toUpper (Data.Text.pack "POST")) (Data.Text.pack "/user/createWithList") [] body OpenAPI.Common.RequestBodyEncodingJSON)
-- | > POST /user/createWithList
-- 
-- Monadic version of 'createUsersWithListInputRaw' (use with 'OpenAPI.Common.runWithConfiguration')
createUsersWithListInputRawM :: forall m s . (OpenAPI.Common.MonadHTTP m,
                                              OpenAPI.Common.SecurityScheme s) =>
                                [] User ->
                                Control.Monad.Trans.Reader.ReaderT (OpenAPI.Common.Configuration s)
                                                                   m
                                                                   (Data.Either.Either Network.HTTP.Client.Types.HttpException
                                                                                       (Network.HTTP.Client.Types.Response Data.ByteString.Internal.ByteString))
createUsersWithListInputRawM body = GHC.Base.id (OpenAPI.Common.doBodyCallWithConfigurationM (Data.Text.toUpper (Data.Text.pack "POST")) (Data.Text.pack "/user/createWithList") [] body OpenAPI.Common.RequestBodyEncodingJSON)
-- | Represents a response of the operation 'createUsersWithListInput'.
-- 
-- The response constructor is chosen by the status code of the response. If no case matches (no specific case for the response code, no range case, no default case), 'CreateUsersWithListInputResponseError' is used.
data CreateUsersWithListInputResponse =                   
   CreateUsersWithListInputResponseError GHC.Base.String  -- ^ Means either no matching case available or a parse error
  | CreateUsersWithListInputResponseDefault               -- ^ successful operation
  deriving (GHC.Show.Show, GHC.Classes.Eq)
