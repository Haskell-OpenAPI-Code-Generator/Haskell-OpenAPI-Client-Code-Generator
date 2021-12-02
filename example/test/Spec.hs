{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}

import Control.Exception
import Data.Either
import Lib
import Network.HTTP.Client
import Network.HTTP.Simple
import OpenAPI
import Test.Hspec

main :: IO ()
main =
  hspec $ do
    describe "runGetInventory" $
      it "get inventory" $
        do
          response <- runGetInventory
          getResponseBody response
            `shouldSatisfy` ( \case
                                GetInventoryResponse200 _ -> True
                                _ -> False
                            )
    describe "runAddPet" $
      it "add pet" $
        do
          response <- runAddPet
          getResponseBody response
            `shouldBe` AddPetResponse200

    describe "runFindPetsByStatus" $
      it "find pets by status" $
        do
          response <- runFindPetsByStatus
          getResponseBody response
            `shouldSatisfy` ( \case
                                FindPetsByStatusResponse200 _ -> True
                                _ -> False
                            )
