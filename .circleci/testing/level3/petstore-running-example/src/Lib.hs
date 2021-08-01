{-# LANGUAGE OverloadedStrings #-}

module Lib where

import Data.ByteString.Char8
import Network.HTTP.Client
import OpenAPI
import OpenAPI.Common

runAddPet :: MonadHTTP m => m (Response AddPetResponse)
runAddPet =
  runWithConfiguration defaultConfiguration $
    addPet $
      (mkPet [])
        { petId = Just 21,
          petName = Just "Harro",
          petStatus = Just PetStatusEnumAvailable
        }

runGetInventory :: MonadHTTP m => m (Response GetInventoryResponse)
runGetInventory = getInventoryWithConfiguration defaultConfiguration

runFindPetsByStatus :: MonadHTTP m => FindPetsByStatusParametersStatus -> m (Response FindPetsByStatusResponse)
runFindPetsByStatus status = findPetsByStatusWithConfiguration defaultConfiguration [status]
