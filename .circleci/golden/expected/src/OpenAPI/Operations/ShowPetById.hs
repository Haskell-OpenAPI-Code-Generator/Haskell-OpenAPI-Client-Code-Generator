{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ExplicitForAll #-}
{-# LANGUAGE MultiWayIf #-}
{-# LANGUAGE DeriveGeneric #-}

-- | Contains the different functions to run the operation showPetById
module OpenAPI.Operations.ShowPetById where

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
import qualified Data.Vector
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

-- | > GET /pets/{petId}
-- 
-- Info for a specific pet
showPetById :: forall m s . (OpenAPI.Common.MonadHTTP m, OpenAPI.Common.SecurityScheme s) => OpenAPI.Common.Configuration s  -- ^ The configuration to use in the request
  -> Data.Text.Internal.Text                                                                                                 -- ^ petId: The id of the pet to retrieve
  -> m (Data.Either.Either Network.HTTP.Client.Types.HttpException (Network.HTTP.Client.Types.Response ShowPetByIdResponse)) -- ^ Monad containing the result of the operation
showPetById config
            petId = GHC.Base.fmap (GHC.Base.fmap (\response_0 -> GHC.Base.fmap (Data.Either.either ShowPetByIdResponseError GHC.Base.id GHC.Base.. (\response body -> if | GHC.Base.const GHC.Types.True (Network.HTTP.Client.Types.responseStatus response) -> ShowPetByIdResponseDefault Data.Functor.<$> (Data.Aeson.eitherDecodeStrict body :: Data.Either.Either GHC.Base.String
                                                                                                                                                                                                                                                                                                                                                                      Dog)
                                                                                                                                                                         | GHC.Base.otherwise -> Data.Either.Left "Missing default response type") response_0) response_0)) (OpenAPI.Common.doCallWithConfiguration config (Data.Text.toUpper GHC.Base.$ Data.Text.pack "GET") (Data.Text.pack ("/pets/" GHC.Base.++ (Data.ByteString.Char8.unpack (Network.HTTP.Types.URI.urlEncode GHC.Types.True GHC.Base.$ (Data.ByteString.Char8.pack GHC.Base.$ OpenAPI.Common.stringifyModel petId)) GHC.Base.++ ""))) [])
-- | > GET /pets/{petId}
-- 
-- The same as 'showPetById' but returns the raw 'Data.ByteString.Char8.ByteString'
showPetByIdRaw :: forall m s . (OpenAPI.Common.MonadHTTP m,
                                OpenAPI.Common.SecurityScheme s) =>
                  OpenAPI.Common.Configuration s ->
                  Data.Text.Internal.Text ->
                  m (Data.Either.Either Network.HTTP.Client.Types.HttpException
                                        (Network.HTTP.Client.Types.Response Data.ByteString.Internal.ByteString))
showPetByIdRaw config
               petId = GHC.Base.id (OpenAPI.Common.doCallWithConfiguration config (Data.Text.toUpper GHC.Base.$ Data.Text.pack "GET") (Data.Text.pack ("/pets/" GHC.Base.++ (Data.ByteString.Char8.unpack (Network.HTTP.Types.URI.urlEncode GHC.Types.True GHC.Base.$ (Data.ByteString.Char8.pack GHC.Base.$ OpenAPI.Common.stringifyModel petId)) GHC.Base.++ ""))) [])
-- | > GET /pets/{petId}
-- 
-- Monadic version of 'showPetById' (use with 'OpenAPI.Common.runWithConfiguration')
showPetByIdM :: forall m s . (OpenAPI.Common.MonadHTTP m,
                              OpenAPI.Common.SecurityScheme s) =>
                Data.Text.Internal.Text ->
                Control.Monad.Trans.Reader.ReaderT (OpenAPI.Common.Configuration s)
                                                   m
                                                   (Data.Either.Either Network.HTTP.Client.Types.HttpException
                                                                       (Network.HTTP.Client.Types.Response ShowPetByIdResponse))
showPetByIdM petId = GHC.Base.fmap (GHC.Base.fmap (\response_1 -> GHC.Base.fmap (Data.Either.either ShowPetByIdResponseError GHC.Base.id GHC.Base.. (\response body -> if | GHC.Base.const GHC.Types.True (Network.HTTP.Client.Types.responseStatus response) -> ShowPetByIdResponseDefault Data.Functor.<$> (Data.Aeson.eitherDecodeStrict body :: Data.Either.Either GHC.Base.String
                                                                                                                                                                                                                                                                                                                                                                       Dog)
                                                                                                                                                                          | GHC.Base.otherwise -> Data.Either.Left "Missing default response type") response_1) response_1)) (OpenAPI.Common.doCallWithConfigurationM (Data.Text.toUpper GHC.Base.$ Data.Text.pack "GET") (Data.Text.pack ("/pets/" GHC.Base.++ (Data.ByteString.Char8.unpack (Network.HTTP.Types.URI.urlEncode GHC.Types.True GHC.Base.$ (Data.ByteString.Char8.pack GHC.Base.$ OpenAPI.Common.stringifyModel petId)) GHC.Base.++ ""))) [])
-- | > GET /pets/{petId}
-- 
-- Monadic version of 'showPetByIdRaw' (use with 'OpenAPI.Common.runWithConfiguration')
showPetByIdRawM :: forall m s . (OpenAPI.Common.MonadHTTP m,
                                 OpenAPI.Common.SecurityScheme s) =>
                   Data.Text.Internal.Text ->
                   Control.Monad.Trans.Reader.ReaderT (OpenAPI.Common.Configuration s)
                                                      m
                                                      (Data.Either.Either Network.HTTP.Client.Types.HttpException
                                                                          (Network.HTTP.Client.Types.Response Data.ByteString.Internal.ByteString))
showPetByIdRawM petId = GHC.Base.id (OpenAPI.Common.doCallWithConfigurationM (Data.Text.toUpper GHC.Base.$ Data.Text.pack "GET") (Data.Text.pack ("/pets/" GHC.Base.++ (Data.ByteString.Char8.unpack (Network.HTTP.Types.URI.urlEncode GHC.Types.True GHC.Base.$ (Data.ByteString.Char8.pack GHC.Base.$ OpenAPI.Common.stringifyModel petId)) GHC.Base.++ ""))) [])
-- | Represents a response of the operation 'showPetById'.
-- 
-- The response constructor is chosen by the status code of the response. If no case matches (no specific case for the response code, no range case, no default case), 'ShowPetByIdResponseError' is used.
data ShowPetByIdResponse =                   
   ShowPetByIdResponseError GHC.Base.String  -- ^ Means either no matching case available or a parse error
  | ShowPetByIdResponseDefault Dog           -- ^ Expected response to a valid request
  deriving (GHC.Show.Show, GHC.Classes.Eq)
