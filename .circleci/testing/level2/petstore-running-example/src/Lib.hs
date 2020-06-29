{-# LANGUAGE OverloadedStrings #-}

module Lib where

import Control.Monad.Reader
import Data.ByteString.Char8
import Network.HTTP.Client
import OpenAPI
import OpenAPI.Common

myPet :: Pet
myPet =
  Pet
    { petCategory = Nothing,
      petId = Nothing,
      petName = Just "Harro",
      petPhotoUrls = [],
      petStatus = Nothing,
      petTags = Nothing
    }

myTag :: Tag
myTag =
  Tag
    { tagId = Just 3,
      tagName = Just "Tag 1"
    }

bearerConfiguration :: Configuration
bearerConfiguration =
  defaultConfiguration
    { configSecurityScheme = bearerAuthenticationSecurityScheme "token"
    }

runGetInventoryAnonymous :: MonadHTTP m => m (Response GetInventoryResponse)
runGetInventoryAnonymous = getInventoryWithConfiguration defaultConfiguration

runGetInventoryBasicAuth :: MonadHTTP m => m (Response GetInventoryResponse)
runGetInventoryBasicAuth =
  getInventoryWithConfiguration $
    defaultConfiguration
      { configSecurityScheme =
          basicAuthenticationSecurityScheme
            BasicAuthenticationData
              { basicAuthenticationDataUsername = "user",
                basicAuthenticationDataPassword = "pw"
              }
      }

runGetInventoryBearerAuth :: MonadHTTP m => m (Response GetInventoryResponse)
runGetInventoryBearerAuth = getInventoryWithConfiguration bearerConfiguration

runMultipleRequestsWithBearerAuth :: MonadHTTP m => m (Response GetInventoryResponse, Response AddPetResponse)
runMultipleRequestsWithBearerAuth =
  runWithConfiguration bearerConfiguration $ do
    response1 <- getInventory
    response2 <- addPet myPet
    pure (response1, response2)

runAddPet :: MonadHTTP m => m (Response AddPetResponse)
runAddPet = addPetWithConfiguration defaultConfiguration myPet

runGetAllPetsAsOneOf :: MonadHTTP m => m (Response GetAllPetsAsOneOfResponse)
runGetAllPetsAsOneOf = getAllPetsAsOneOfWithConfiguration defaultConfiguration

runUpdatePet :: MonadHTTP m => m (Response UpdatePetResponse)
runUpdatePet = updatePetWithConfiguration defaultConfiguration (UpdatePetRequestBodyPet myPet)

runUpdatePetWithTag :: MonadHTTP m => m (Response UpdatePetResponse)
runUpdatePetWithTag = updatePetWithConfiguration defaultConfiguration (UpdatePetRequestBodyTag myTag)
