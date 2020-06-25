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
    describe "runGetInventory"
      $ it "get inventory"
      $ do
        response <- runGetInventory
        getResponseBody response
          `shouldSatisfy` ( \case
                              GetInventoryResponse200 _ -> True
                              _ -> False
                          )
    describe "runAddPetAndFindIt"
      $ it "add pet and find it"
      $ do
        (response1, response2) <- runAddPetAndFindIt
        getResponseBody response1
          `shouldBe` AddPetResponse200
        getResponseBody response2
          `shouldSatisfy` ( \case
                              FindPetsByStatusResponse200 pets -> any (\p -> petName p == "Harro") pets
                              _ -> False
                          )
    describe "runFindPetsByStatus"
      $ it "find pets by status"
      $ do
        response <- runFindPetsByStatus
        getResponseBody response
          `shouldSatisfy` ( \case
                              FindPetsByStatusResponse200 _ -> True
                              _ -> False
                          )
