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
          { infoObjectTitle = "",
            infoObjectDescription = Nothing,
            infoObjectTermsOfService = Nothing,
            infoObjectContact = Nothing,
            infoObjectLicense = Nothing,
            infoObjectVersion = "0.0.1"
          }
      e = OAT.ExampleObject {exampleObjectSummary = Just "foo", exampleObjectDescription = Nothing, exampleObjectValue = Nothing, exampleObjectExternalValue = Just "http://example.com"}
      c =
        OAT.ComponentsObject
          { componentsObjectSchemas = Map.empty,
            componentsObjectResponses = Map.empty,
            componentsObjectParameters = Map.empty,
            componentsObjectExamples =
              Map.fromList
                [ ("example1", OAT.Concrete e),
                  ("example2", OAT.Reference "#/components/examples/example1")
                ],
            componentsObjectRequestBodies = Map.empty,
            componentsObjectHeaders = Map.empty,
            componentsObjectSecuritySchemes = Map.empty
          }
      openApiSpec =
        OAT.OpenApiSpecification
          { openApiSpecificationOpenapi = "3.0.3",
            openApiSpecificationInfo = i,
            openApiSpecificationServers = [],
            openApiSpecificationPaths = Map.empty,
            openApiSpecificationComponents = c,
            openApiSpecificationSecurity = [],
            openApiSpecificationTags = [],
            openApiSpecificationExternalDocs = Nothing
          }
  describe "buildReferenceMap" $ do
    it "should find reference" $
      getExampleReference "#/components/examples/example1" (buildReferenceMap openApiSpec)
        `shouldBe` Just e
    it "should not find reference pointing to other reference" $
      getExampleReference "#/components/examples/example2" (buildReferenceMap openApiSpec) `shouldBe` Nothing
    it "should not find unexisting reference" $
      getExampleReference "#/components/examples/example99" (buildReferenceMap openApiSpec) `shouldBe` Nothing
