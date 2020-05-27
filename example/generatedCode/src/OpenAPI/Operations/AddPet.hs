{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ExplicitForAll #-}
{-# LANGUAGE MultiWayIf #-}
{-# LANGUAGE DeriveGeneric #-}

-- | Contains the different functions to run the operation addPet
module OpenAPI.Operations.AddPet where

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

-- | > POST /pet
-- 
-- 
addPet :: forall m s . (OpenAPI.Common.MonadHTTP m, OpenAPI.Common.SecurityScheme s) => OpenAPI.Common.Configuration s  -- ^ The configuration to use in the request
  -> Pet                                                                                                                -- ^ The request body to send
  -> m (Data.Either.Either Network.HTTP.Client.Types.HttpException (Network.HTTP.Client.Types.Response AddPetResponse)) -- ^ Monad containing the result of the operation
addPet config
       body = GHC.Base.fmap (GHC.Base.fmap (\response_0 -> GHC.Base.fmap (Data.Either.either AddPetResponseError GHC.Base.id GHC.Base.. (\response body -> if | (\status_1 -> Network.HTTP.Types.Status.statusCode status_1 GHC.Classes.== 200) (Network.HTTP.Client.Types.responseStatus response) -> Data.Either.Right AddPetResponse200
                                                                                                                                                              | (\status_2 -> Network.HTTP.Types.Status.statusCode status_2 GHC.Classes.== 405) (Network.HTTP.Client.Types.responseStatus response) -> Data.Either.Right AddPetResponse405
                                                                                                                                                              | GHC.Base.otherwise -> Data.Either.Left "Missing default response type") response_0) response_0)) (OpenAPI.Common.doBodyCallWithConfiguration config (Data.Text.toUpper (Data.Text.pack "POST")) (Data.Text.pack "/pet") [] body OpenAPI.Common.RequestBodyEncodingJSON)
-- | > POST /pet
-- 
-- The same as 'addPet' but returns the raw 'Data.ByteString.Char8.ByteString'
addPetRaw :: forall m s . (OpenAPI.Common.MonadHTTP m,
                           OpenAPI.Common.SecurityScheme s) =>
             OpenAPI.Common.Configuration s ->
             Pet ->
             m (Data.Either.Either Network.HTTP.Client.Types.HttpException
                                   (Network.HTTP.Client.Types.Response Data.ByteString.Internal.ByteString))
addPetRaw config
          body = GHC.Base.id (OpenAPI.Common.doBodyCallWithConfiguration config (Data.Text.toUpper (Data.Text.pack "POST")) (Data.Text.pack "/pet") [] body OpenAPI.Common.RequestBodyEncodingJSON)
-- | > POST /pet
-- 
-- Monadic version of 'addPet' (use with 'OpenAPI.Common.runWithConfiguration')
addPetM :: forall m s . (OpenAPI.Common.MonadHTTP m,
                         OpenAPI.Common.SecurityScheme s) =>
           Pet ->
           Control.Monad.Trans.Reader.ReaderT (OpenAPI.Common.Configuration s)
                                              m
                                              (Data.Either.Either Network.HTTP.Client.Types.HttpException
                                                                  (Network.HTTP.Client.Types.Response AddPetResponse))
addPetM body = GHC.Base.fmap (GHC.Base.fmap (\response_3 -> GHC.Base.fmap (Data.Either.either AddPetResponseError GHC.Base.id GHC.Base.. (\response body -> if | (\status_4 -> Network.HTTP.Types.Status.statusCode status_4 GHC.Classes.== 200) (Network.HTTP.Client.Types.responseStatus response) -> Data.Either.Right AddPetResponse200
                                                                                                                                                               | (\status_5 -> Network.HTTP.Types.Status.statusCode status_5 GHC.Classes.== 405) (Network.HTTP.Client.Types.responseStatus response) -> Data.Either.Right AddPetResponse405
                                                                                                                                                               | GHC.Base.otherwise -> Data.Either.Left "Missing default response type") response_3) response_3)) (OpenAPI.Common.doBodyCallWithConfigurationM (Data.Text.toUpper (Data.Text.pack "POST")) (Data.Text.pack "/pet") [] body OpenAPI.Common.RequestBodyEncodingJSON)
-- | > POST /pet
-- 
-- Monadic version of 'addPetRaw' (use with 'OpenAPI.Common.runWithConfiguration')
addPetRawM :: forall m s . (OpenAPI.Common.MonadHTTP m,
                            OpenAPI.Common.SecurityScheme s) =>
              Pet ->
              Control.Monad.Trans.Reader.ReaderT (OpenAPI.Common.Configuration s)
                                                 m
                                                 (Data.Either.Either Network.HTTP.Client.Types.HttpException
                                                                     (Network.HTTP.Client.Types.Response Data.ByteString.Internal.ByteString))
addPetRawM body = GHC.Base.id (OpenAPI.Common.doBodyCallWithConfigurationM (Data.Text.toUpper (Data.Text.pack "POST")) (Data.Text.pack "/pet") [] body OpenAPI.Common.RequestBodyEncodingJSON)
-- | Represents a response of the operation 'addPet'.
-- 
-- The response constructor is chosen by the status code of the response. If no case matches (no specific case for the response code, no range case, no default case), 'AddPetResponseError' is used.
data AddPetResponse =                      
   AddPetResponseError GHC.Base.String     -- ^ Means either no matching case available or a parse error
  | AddPetResponse200                      -- ^ Success
  | AddPetResponse405                      -- ^ Invalid input
  deriving (GHC.Show.Show, GHC.Classes.Eq)
