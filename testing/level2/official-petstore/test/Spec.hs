{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}

import Control.Monad.Trans.Reader
import Data.ByteString.Char8
import Data.Either
import Lib
import Network.HTTP.Client.Internal
import Network.HTTP.Simple
import Network.HTTP.Types
import OpenAPI
import Test.Hspec
import Test.MonadHTTP

main :: IO ()
main =
  hspec $ do
    describe "runListPets" $ do
      it "get pets" $ do
        let requestExpectation = expectURL "http://petstore.swagger.io/v1/pets" $ expectMethod "GET" noExpectation
            rawResponse = defaultResponse {responseBody = "[{\"id\":12,\"name\":\"foo\"}]"} :: Response ByteString
        response <- runMock runListPets (requestExpectation, rawResponse)
        getResponseBody response `shouldBe` ListPetsResponse200 [Pet {petId = 12, petName = "foo", petTag = Nothing}]
      it "should fail with wrong expectations" $ do
        let requestExpectation = expectMethod "POST" noExpectation
        runMock runListPets (requestExpectation, defaultResponse) `shouldThrow` (== MethodMismatch "GET")
    describe "runCreatePets" $ do
      it "creates pet" $ do
        let requestExpectation = expectURL "http://petstore.swagger.io/v1/pets" $ expectMethod "POST" noExpectation
            rawResponse = defaultResponse {responseStatus = created201} :: Response ByteString
        response <- runMock runCreatePets (requestExpectation, rawResponse)
        getResponseBody response `shouldBe` CreatePetsResponse201
      it "fails to create" $ do
        let rawResponse = defaultResponse {responseStatus = badRequest400, responseBody = "{\"code\":100,\"message\":\"error100\"}"} :: Response ByteString
        response <- runMock runCreatePets (noExpectation, rawResponse)
        getResponseBody response `shouldBe` CreatePetsResponseDefault (Error {errorCode = 100, errorMessage = "error100"})
      it "fails to parse" $ do
        let rawResponse = defaultResponse {responseStatus = badRequest400, responseBody = "not valid JSON"} :: Response ByteString
        response <- runMock runCreatePets (noExpectation, rawResponse)
        getResponseBody response
          `shouldSatisfy` ( \case
                              CreatePetsResponseError _ -> True
                              _ -> False
                          )
    describe "runShowPetById" $ do
      it "show pet" $ do
        let requestExpectation = expectURL "http://petstore.swagger.io/v1/pets/7" $ expectMethod "GET" noExpectation
            rawResponse = defaultResponse {responseBody = "{\"id\":1,\"name\":\"bar\",\"tag\":\"some\"}"} :: Response ByteString
        response <- runMock (runShowPetById "7") (requestExpectation, rawResponse)
        getResponseBody response `shouldBe` ShowPetByIdResponse200 (Pet {petId = 1, petName = "bar", petTag = Just "some"})
      it "show pet with quirky id" $ do
        let requestExpectation = expectURL "http://petstore.swagger.io/v1/pets/a%2F%20%25%23" noExpectation
            rawResponse = defaultResponse {responseBody = "{\"id\":1,\"name\":\"bar\",\"tag\":\"some\"}"} :: Response ByteString
        response <- runMock (runShowPetById "a/ %#") (requestExpectation, rawResponse)
        getResponseBody response `shouldBe` ShowPetByIdResponse200 (Pet {petId = 1, petName = "bar", petTag = Just "some"})
