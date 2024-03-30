{-# LANGUAGE OverloadedStrings #-}

module Lib where

import Data.ByteString.Char8
import Data.Text
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

runEchoUserAgent :: MonadHTTP m => m (Response EchoUserAgentResponse)
runEchoUserAgent =
  echoUserAgentWithConfiguration $
    defaultConfiguration
      { configApplicationName = "XYZ"
      }

runEchoUserAgentWithoutUserAgent :: MonadHTTP m => m (Response EchoUserAgentResponse)
runEchoUserAgentWithoutUserAgent =
  echoUserAgentWithConfiguration $
    defaultConfiguration
      { configIncludeUserAgent = False
      }

runEchoPath :: MonadHTTP m => EchoPathParametersPath -> m (Response EchoPathResponse)
runEchoPath = echoPathWithConfiguration defaultConfiguration

runSendAndReceiveNullableAndOptional :: MonadHTTP m => Text -> NullableAndOptionalTest -> m (Response SendAndReceiveNullableAndOptionalResponse)
runSendAndReceiveNullableAndOptional = sendAndReceiveNullableAndOptionalWithConfiguration defaultConfiguration
