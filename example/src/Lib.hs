{-# LANGUAGE OverloadedStrings #-}

module Lib where

import Data.ByteString.Char8
import Network.HTTP.Client
import OpenAPI
import OpenAPI.Common

runAddPet :: (MonadHTTP m) => m (Response AddPetResponse)
runAddPet = runWithConfiguration defaultConfiguration (addPet myPet)
  where
    myPet =
      Pet
        { petCategory = Nothing,
          petId = Nothing,
          petName = "Harro",
          petPhotoUrls = [],
          petStatus = Nothing,
          petTags = Nothing
        }

runGetInventory :: (MonadHTTP m) => m (Response GetInventoryResponse)
runGetInventory = runWithConfiguration defaultConfiguration getInventory

runFindPetsByStatus :: (MonadHTTP m) => m (Response FindPetsByStatusResponse)
runFindPetsByStatus =
  runWithConfiguration
    defaultConfiguration
    (findPetsByStatus [FindPetsByStatusParametersStatusEnumPending])
