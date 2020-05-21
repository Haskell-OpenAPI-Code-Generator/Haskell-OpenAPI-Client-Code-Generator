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
        testOperation
          { summary = Just "my summary"
          } ::
          OAT.OperationObject
      testOperation3 =
        testOperation
          { description = Just "my description"
          } ::
          OAT.OperationObject
      testOperation4 =
        testOperation
          { summary = Just "my summary",
            description = Just "my description"
          } ::
          OAT.OperationObject
   in describe "getOperationDesciption" $ do
        it
          "should return an empty string"
          (getOperationDescription testOperation `shouldBe` "")
        it
          "should return description"
          (getOperationDescription testOperation2 `shouldBe` "my summary")
        it
          "should return summary"
          (getOperationDescription testOperation3 `shouldBe` "my description")
        it
          "should return description"
          (getOperationDescription testOperation4 `shouldBe` "my description")
