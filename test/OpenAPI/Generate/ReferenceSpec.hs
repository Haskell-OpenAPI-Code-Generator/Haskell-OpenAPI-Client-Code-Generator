{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE OverloadedStrings #-}

module OpenAPI.Generate.ReferenceSpec where

import qualified Data.Map as Map
import OpenAPI.Generate.Reference
import OpenAPI.Generate.Types as OAT
import Test.Hspec

spec :: Spec
spec = do
  let i =
        OAT.InfoObject
          { title = "",
            description = Nothing,
            termsOfService = Nothing,
            contact = Nothing,
            license = Nothing,
            version = "0.0.1"
          }
      e = OAT.ExampleObject {summary = Just "foo", description = Nothing, value = Nothing, externalValue = Just "http://example.com"}
      c =
        OAT.ComponentsObject
          { schemas = Map.empty,
            responses = Map.empty,
            parameters = Map.empty,
            examples =
              Map.fromList
                [ ("example1", OAT.Concrete e),
                  ("example2", OAT.Reference "#/components/examples/example1")
                ],
            requestBodies = Map.empty,
            headers = Map.empty,
            securitySchemes = Map.empty
          }
      openApiSpec =
        OAT.OpenApiSpecification
          { openapi = "3.0.3",
            info = i,
            servers = [],
            paths = Map.empty,
            components = c,
            security = [],
            tags = [],
            externalDocs = Nothing
          }
  describe "buildReferenceMap" $ do
    it "should find reference" $
      getExampleReference "#/components/examples/example1" (buildReferenceMap openApiSpec)
        `shouldBe` Just e
    it "should not find reference pointing to other reference" $
      getExampleReference "#/components/examples/example2" (buildReferenceMap openApiSpec) `shouldBe` Nothing
    it "should not find unexisting reference" $
      getExampleReference "#/components/examples/example99" (buildReferenceMap openApiSpec) `shouldBe` Nothing
