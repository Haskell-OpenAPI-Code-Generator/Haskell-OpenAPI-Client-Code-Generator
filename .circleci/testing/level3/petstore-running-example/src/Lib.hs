{-# LANGUAGE OverloadedStrings #-}

module Lib where

import Data.ByteString.Char8
import Network.HTTP.Client
import OpenAPI
import OpenAPI.Common

runAddPetAndFindIt :: MonadHTTP m => m (Response AddPetResponse, Response FindPetsByStatusResponse)
runAddPetAndFindIt = runWithConfiguration defaultConfiguration $ do
  response1 <- addPet myPet
  response2 <- findPetsByStatus [FindPetsByStatusParametersStatusEnumString_pending]
  pure (response1, response2)
  where
    myPet =
      Pet
        { petCategory = Nothing,
          petId = Nothing,
          petName = Just "Harro",
          petPhotoUrls = [],
          petStatus = Just PetStatusEnumString_pending,
          petTags = Nothing
        }

runGetInventory :: MonadHTTP m => m (Response GetInventoryResponse)
runGetInventory = getInventoryWithConfiguration defaultConfiguration

runFindPetsByStatus :: MonadHTTP m => m (Response FindPetsByStatusResponse)
runFindPetsByStatus = findPetsByStatusWithConfiguration defaultConfiguration [FindPetsByStatusParametersStatusEnumString_pending]
