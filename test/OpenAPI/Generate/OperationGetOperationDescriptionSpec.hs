{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE OverloadedStrings #-}

module OpenAPI.Generate.OperationGetOperationDescriptionSpec where

import qualified Data.Map as Map
import OpenAPI.Generate.Internal.Operation
import OpenAPI.Generate.Types as OAT
import Test.Hspec

spec :: Spec
spec =
  let emptyResponseObject =
        OAT.ResponsesObject
          { default' = Nothing,
            range1XX = Nothing,
            range2XX = Nothing,
            range3XX = Nothing,
            range4XX = Nothing,
            range5XX = Nothing,
            perStatusCode = Map.empty
          }
      testOperation =
        OAT.OperationObject
          { tags = [],
            summary = Nothing,
            description = Nothing,
            externalDocs = Nothing,
            operationId = Nothing,
            parameters = [],
            requestBody = Nothing,
            responses = emptyResponseObject,
            deprecated = False,
            security = [],
            servers = []
          }
      testOperation2 =
        OAT.OperationObject
          { tags = [],
            summary = Just "my description",
            description = Nothing,
            externalDocs = Nothing,
            operationId = Nothing,
            parameters = [],
            requestBody = Nothing,
            responses = emptyResponseObject,
            deprecated = False,
            security = [],
            servers = []
          }
   in describe "getOperationDesciption" $ do
        it
          "should return a indicator for empty value"
          (getOperationDescription testOperation `shouldBe` "No summary provided")
        it
          "should return description"
          (getOperationDescription testOperation2 `shouldBe` "my description")
