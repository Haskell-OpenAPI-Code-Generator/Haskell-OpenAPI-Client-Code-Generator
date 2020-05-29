{-# LANGUAGE OverloadedStrings #-}

module Lib where

import Data.ByteString.Char8
import Network.HTTP.Client
import OpenAPI
import OpenAPI.Common

runAddPet :: MonadHTTP m => m (Either HttpException (Response AddPetResponse))
runAddPet = addPet defaultConfiguration myPet
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

runGetInventory :: MonadHTTP m => m (Either HttpException (Response GetInventoryResponse))
runGetInventory = getInventory defaultConfiguration

runFindPetsByStatus :: MonadHTTP m => m (Either HttpException (Response FindPetsByStatusResponse))
runFindPetsByStatus = findPetsByStatus defaultConfiguration "pending"
