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
      petName = "Harro",
      petPhotoUrls = [],
      petStatus = Nothing,
      petTags = Nothing
    }

bearerConfiguration :: Configuration BearerAuthenticationSecurityScheme
bearerConfiguration =
  defaultConfiguration
    { configSecurityScheme = BearerAuthenticationSecurityScheme "token"
    }

runGetInventoryAnonymous :: MonadHTTP m => m (Either HttpException (Response GetInventoryResponse))
runGetInventoryAnonymous = getInventory defaultConfiguration

runGetInventoryBasicAuth :: MonadHTTP m => m (Either HttpException (Response GetInventoryResponse))
runGetInventoryBasicAuth =
  getInventory $
    defaultConfiguration
      { configSecurityScheme =
          BasicAuthenticationSecurityScheme
            { basicAuthenticationSecuritySchemeUsername = "user",
              basicAuthenticationSecuritySchemePassword = "pw"
            }
      }

runGetInventoryBearerAuth :: MonadHTTP m => m (Either HttpException (Response GetInventoryResponse))
runGetInventoryBearerAuth = getInventory bearerConfiguration

runMultipleRequestsWithBearerAuth :: MonadHTTP m => m (Either HttpException (Response GetInventoryResponse), Either HttpException (Response AddPetResponse))
runMultipleRequestsWithBearerAuth =
  runWithConfiguration bearerConfiguration $ do
    response1 <- getInventoryM
    response2 <- addPetM myPet
    pure (response1, response2)

runAddPet :: MonadHTTP m => m (Either HttpException (Response AddPetResponse))
runAddPet = addPet defaultConfiguration myPet
