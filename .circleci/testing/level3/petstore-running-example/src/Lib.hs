{-# LANGUAGE OverloadedStrings #-}

module Lib where

import Data.ByteString.Char8
import Network.HTTP.Client
import OpenAPI
import OpenAPI.Common

runAddPetAndFindIt :: MonadHTTP m => m (Response AddPetResponse, Response FindPetsByStatusResponse)
runAddPetAndFindIt = runWithConfiguration defaultConfiguration $ do
  response1 <- addPet myPet
  response2 <- findPetsByStatus [FindPetsByStatusParametersStatusEnumPending]
  pure (response1, response2)
  where
    myPet =
      (mkPet [])
        { petName = Just "Harro",
          petStatus = Just PetStatusEnumPending
        }

runGetInventory :: MonadHTTP m => m (Response GetInventoryResponse)
runGetInventory = getInventoryWithConfiguration defaultConfiguration

runFindPetsByStatus :: MonadHTTP m => m (Response FindPetsByStatusResponse)
runFindPetsByStatus = findPetsByStatusWithConfiguration defaultConfiguration [FindPetsByStatusParametersStatusEnumPending]
