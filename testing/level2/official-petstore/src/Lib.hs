module Lib where

import Data.ByteString.Char8
import Data.Text (Text)
import Network.HTTP.Client
import OpenAPI
import OpenAPI.Common

runListPets :: MonadHTTP m => m (Response ListPetsResponse)
runListPets = listPetsWithConfiguration defaultConfiguration Nothing

runCreatePets :: MonadHTTP m => m (Response CreatePetsResponse)
runCreatePets = createPetsWithConfiguration defaultConfiguration

runShowPetById :: MonadHTTP m => Text -> m (Response ShowPetByIdResponse)
runShowPetById = showPetByIdWithConfiguration defaultConfiguration
