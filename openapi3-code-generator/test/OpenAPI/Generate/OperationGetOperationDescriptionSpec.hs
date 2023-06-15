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
          { responsesObjectDefault = Nothing,
            responsesObjectRange1XX = Nothing,
            responsesObjectRange2XX = Nothing,
            responsesObjectRange3XX = Nothing,
            responsesObjectRange4XX = Nothing,
            responsesObjectRange5XX = Nothing,
            responsesObjectPerStatusCode = Map.empty
          }
      testOperation =
        OAT.OperationObject
          { operationObjectTags = [],
            operationObjectSummary = Nothing,
            operationObjectDescription = Nothing,
            operationObjectExternalDocs = Nothing,
            operationObjectOperationId = Nothing,
            operationObjectParameters = [],
            operationObjectRequestBody = Nothing,
            operationObjectResponses = emptyResponseObject,
            operationObjectDeprecated = False,
            operationObjectSecurity = [],
            operationObjectServers = []
          }
      testOperation2 =
        testOperation
          { operationObjectSummary = Just "my summary"
          }
      testOperation3 =
        testOperation
          { operationObjectDescription = Just "my description"
          }
      testOperation4 =
        testOperation
          { operationObjectSummary = Just "my summary",
            operationObjectDescription = Just "my description"
          }
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
