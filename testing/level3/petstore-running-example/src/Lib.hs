{-# LANGUAGE OverloadedStrings #-}

module Lib where

import Data.ByteString.Char8
import Data.Text
import Network.HTTP.Client
import OpenAPI
import OpenAPI.Common

runAddPet :: (MonadHTTP m) => m (Response AddPetResponse)
runAddPet =
  runWithConfiguration defaultConfiguration $
    addPet $
      (mkPet [])
        { petId = Just 21,
          petName = Just "Harro",
          petStatus = Just PetStatus'EnumAvailable
        }

runGetInventory :: (MonadHTTP m) => m (Response GetInventoryResponse)
runGetInventory = runWithConfiguration defaultConfiguration getInventory

runFindPetsByStatus :: (MonadHTTP m) => FindPetsByStatusParametersStatus -> m (Response FindPetsByStatusResponse)
runFindPetsByStatus status = runWithConfiguration defaultConfiguration $ findPetsByStatus [status]

runEchoUserAgent :: (MonadHTTP m) => m (Response EchoUserAgentResponse)
runEchoUserAgent =
  runWithConfiguration
    defaultConfiguration
      { configApplicationName = "XYZ"
      }
    echoUserAgent

runEchoUserAgentWithoutUserAgent :: (MonadHTTP m) => m (Response EchoUserAgentResponse)
runEchoUserAgentWithoutUserAgent =
  runWithConfiguration
    defaultConfiguration
      { configIncludeUserAgent = False
      }
    echoUserAgent

runEchoPath :: (MonadHTTP m) => EchoPathParametersPath -> m (Response EchoPathResponse)
runEchoPath path = runWithConfiguration defaultConfiguration $ echoPath path

runSendAndReceiveNullableAndOptional :: (MonadHTTP m) => Text -> NullableAndOptionalTest -> m (Response SendAndReceiveNullableAndOptionalResponse)
runSendAndReceiveNullableAndOptional mode body = runWithConfiguration defaultConfiguration $ sendAndReceiveNullableAndOptional mode body

runGetFishByType :: (MonadHTTP m) => Text -> m (Response GetFishByTypeResponse)
runGetFishByType fishType = runWithConfiguration defaultConfiguration $ getFishByType fishType

runGetLizardByType :: (MonadHTTP m) => Text -> m (Response GetLizardByTypeResponse)
runGetLizardByType lizardType = runWithConfiguration defaultConfiguration $ getLizardByType lizardType
