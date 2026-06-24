{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}

import Control.Exception
import Data.Aeson hiding (Null)
import Data.Either
import qualified Data.HashMap.Strict as HM
import qualified Data.Text as T
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
            `shouldBe` GetInventoryResponse200
              ( HM.fromList
                  [ ("pet1", Number 23),
                    ("pet2", Number 2)
                  ]
              )
    describe "runAddPet" $
      it "add pet" $
        do
          response <- runAddPet
          getResponseBody response `shouldBe` AddPetResponse200
    describe "runFindPetsByStatus" $
      it "find pets by status" $
        do
          response <- runFindPetsByStatus FindPetsByStatusParametersStatusEnumPending
          getResponseBody response
            `shouldBe` FindPetsByStatusResponse200
              [ (mkPet [])
                  { petId = Just 23,
                    petName = Just "Ted",
                    petStatus = Just PetStatus'EnumPending
                  }
              ]
    describe "runFindPetsByStatus" $
      it "find pets by status" $
        do
          response <- runFindPetsByStatus $ FindPetsByStatusParametersStatusTyped "notExistingStatus"
          getResponseBody response
            `shouldBe` FindPetsByStatusResponse400

    describe "runEchoUserAgent" $
      it "returns the user agent" $
        do
          response <- runEchoUserAgent
          getResponseBody response
            `shouldSatisfy` ( \case
                                EchoUserAgentResponse200 text ->
                                  "XYZ"
                                    `T.isInfixOf` text
                                    && "openapi3-code-generator"
                                      `T.isInfixOf` text
                                _ -> False
                            )

    describe "runEchoUserAgentWithoutUserAgent" $
      it "should fail as the user agent is not present" $
        do
          response <- runEchoUserAgentWithoutUserAgent
          getResponseBody response
            `shouldBe` EchoUserAgentResponse400

    describe "runEchoPath" $ do
      it "returns enum value 'test'" $
        do
          response <- runEchoPath EchoPathParametersPathEnumTest
          getResponseBody response
            `shouldBe` EchoPathResponse200 "test"
      it "returns enum value 'foo'" $
        do
          response <- runEchoPath EchoPathParametersPathEnumFoo
          getResponseBody response
            `shouldBe` EchoPathResponse200 "foo"
      it "works with custom value" $
        do
          response <- runEchoPath $ EchoPathParametersPathTyped "xyz"
          getResponseBody response
            `shouldBe` EchoPathResponse200 "xyz"

    describe "runSendAndReceiveNullableAndOptional" $ do
      it "should work with filled objects" $ do
        response <-
          runSendAndReceiveNullableAndOptional
            "filled"
            NullableAndOptionalTest
              { nullableAndOptionalTestRequiredNonNullable = "x",
                nullableAndOptionalTestRequiredNullable = NonNull "x",
                nullableAndOptionalTestOptionalNonNullable = Just "x",
                nullableAndOptionalTestOptionalNullable = Just (NonNull "x"),
                nullableAndOptionalTestReferencedRequiredNonNullable = "x",
                nullableAndOptionalTestReferencedRequiredNullable = NonNull "x",
                nullableAndOptionalTestReferencedOptionalNonNullable = Just "x",
                nullableAndOptionalTestReferencedOptionalNullable = Just (NonNull "x")
              }
        getResponseBody response
          `shouldBe` SendAndReceiveNullableAndOptionalResponse200
            NullableAndOptionalTest
              { nullableAndOptionalTestRequiredNonNullable = "x",
                nullableAndOptionalTestRequiredNullable = NonNull "x",
                nullableAndOptionalTestOptionalNonNullable = Just "x",
                nullableAndOptionalTestOptionalNullable = Just (NonNull "x"),
                nullableAndOptionalTestReferencedRequiredNonNullable = "x",
                nullableAndOptionalTestReferencedRequiredNullable = NonNull "x",
                nullableAndOptionalTestReferencedOptionalNonNullable = Just "x",
                nullableAndOptionalTestReferencedOptionalNullable = Just (NonNull "x")
              }
      it "should work with null values where possible" $ do
        response <-
          runSendAndReceiveNullableAndOptional
            "emptyNull"
            NullableAndOptionalTest
              { nullableAndOptionalTestRequiredNonNullable = "x",
                nullableAndOptionalTestRequiredNullable = Null,
                nullableAndOptionalTestOptionalNonNullable = Nothing,
                nullableAndOptionalTestOptionalNullable = Just Null,
                nullableAndOptionalTestReferencedRequiredNonNullable = "x",
                nullableAndOptionalTestReferencedRequiredNullable = Null,
                nullableAndOptionalTestReferencedOptionalNonNullable = Nothing,
                nullableAndOptionalTestReferencedOptionalNullable = Just Null
              }
        getResponseBody response
          `shouldBe` SendAndReceiveNullableAndOptionalResponse200
            NullableAndOptionalTest
              { nullableAndOptionalTestRequiredNonNullable = "x",
                nullableAndOptionalTestRequiredNullable = Null,
                nullableAndOptionalTestOptionalNonNullable = Nothing,
                nullableAndOptionalTestOptionalNullable = Just Null,
                nullableAndOptionalTestReferencedRequiredNonNullable = "x",
                nullableAndOptionalTestReferencedRequiredNullable = Null,
                nullableAndOptionalTestReferencedOptionalNonNullable = Nothing,
                nullableAndOptionalTestReferencedOptionalNullable = Just Null
              }
      it "should work with absent values where possible" $ do
        response <-
          runSendAndReceiveNullableAndOptional
            "emptyAbsent"
            NullableAndOptionalTest
              { nullableAndOptionalTestRequiredNonNullable = "x",
                nullableAndOptionalTestRequiredNullable = Null,
                nullableAndOptionalTestOptionalNonNullable = Nothing,
                nullableAndOptionalTestOptionalNullable = Nothing,
                nullableAndOptionalTestReferencedRequiredNonNullable = "x",
                nullableAndOptionalTestReferencedRequiredNullable = Null,
                nullableAndOptionalTestReferencedOptionalNonNullable = Nothing,
                nullableAndOptionalTestReferencedOptionalNullable = Nothing
              }
        getResponseBody response
          `shouldBe` SendAndReceiveNullableAndOptionalResponse200
            NullableAndOptionalTest
              { nullableAndOptionalTestRequiredNonNullable = "x",
                nullableAndOptionalTestRequiredNullable = Null,
                nullableAndOptionalTestOptionalNonNullable = Nothing,
                nullableAndOptionalTestOptionalNullable = Nothing,
                nullableAndOptionalTestReferencedRequiredNonNullable = "x",
                nullableAndOptionalTestReferencedRequiredNullable = Null,
                nullableAndOptionalTestReferencedOptionalNonNullable = Nothing,
                nullableAndOptionalTestReferencedOptionalNullable = Nothing
              }
      describe "expected parse errors" $ do
        let defaultRequestBodyWhichIsIgnored =
              NullableAndOptionalTest
                { nullableAndOptionalTestRequiredNonNullable = "x",
                  nullableAndOptionalTestRequiredNullable = NonNull "x",
                  nullableAndOptionalTestOptionalNonNullable = Just "x",
                  nullableAndOptionalTestOptionalNullable = Just (NonNull "x"),
                  nullableAndOptionalTestReferencedRequiredNonNullable = "x",
                  nullableAndOptionalTestReferencedRequiredNullable = NonNull "x",
                  nullableAndOptionalTestReferencedOptionalNonNullable = Just "x",
                  nullableAndOptionalTestReferencedOptionalNullable = Just (NonNull "x")
                }

        it "should fail on null value for required key" $ do
          response <- runSendAndReceiveNullableAndOptional "errorRequiredNonNullableWithNull" defaultRequestBodyWhichIsIgnored
          getResponseBody response
            `shouldBe` SendAndReceiveNullableAndOptionalResponseError
              "Error in $.requiredNonNullable: parsing Text failed, expected String, but encountered Null"

        it "should fail on no value for required key" $ do
          response <- runSendAndReceiveNullableAndOptional "errorRequiredNonNullableWithAbsence" defaultRequestBodyWhichIsIgnored
          getResponseBody response
            `shouldBe` SendAndReceiveNullableAndOptionalResponseError
              "Error in $: key \"requiredNonNullable\" not found"

        it "should fail on no value for required nullable key" $ do
          response <- runSendAndReceiveNullableAndOptional "errorRequiredNullable" defaultRequestBodyWhichIsIgnored
          getResponseBody response
            `shouldBe` SendAndReceiveNullableAndOptionalResponseError
              "Error in $: key \"requiredNullable\" not found"

        it "should fail on null value for optional non-nullable key" $ do
          response <- runSendAndReceiveNullableAndOptional "errorOptionalNonNullable" defaultRequestBodyWhichIsIgnored
          getResponseBody response
            `shouldBe` SendAndReceiveNullableAndOptionalResponseError
              "Error in $.optionalNonNullable: parsing Text failed, expected String, but encountered Null"

    describe "Discriminator with mapping" $ do
      describe "runGetFishByType" $ do
        it "correctly parses guppie variant using discriminator" $ do
          response <- runGetFishByType "guppie"
          case getResponseBody response of
            GetFishByTypeResponse200 (FishGuppie guppie) -> do
              guppieFishType guppie `shouldBe` "guppie"
              guppieColor guppie `shouldBe` Just "orange"
            other -> expectationFailure $ "Expected Guppie variant, got: " ++ show other

        it "correctly parses minnow variant using discriminator" $ do
          response <- runGetFishByType "minnow"
          case getResponseBody response of
            GetFishByTypeResponse200 (FishMinnow minnow) -> do
              minnowFishType minnow `shouldBe` "minnow"
              minnowColor minnow `shouldBe` Just "silver"
            other -> expectationFailure $ "Expected Minnow variant, got: " ++ show other

        it "correctly parses shark variant using discriminator" $ do
          response <- runGetFishByType "shark"
          case getResponseBody response of
            GetFishByTypeResponse200 (FishShark shark) -> do
              sharkFishType shark `shouldBe` "shark"
              sharkTeethRemaining shark `shouldBe` Just 42
            other -> expectationFailure $ "Expected Shark variant, got: " ++ show other

        it "returns error for invalid fish type" $ do
          response <- runGetFishByType "invalidFish"
          case getResponseBody response of
            GetFishByTypeResponseError _ -> pure ()
            GetFishByTypeResponse400 -> pure ()
            other -> expectationFailure $ "Expected error response, got: " ++ show other

    describe "Discriminator without mapping" $ do
      describe "runGetLizardByType" $ do
        it "correctly parses gecko variant using discriminator (no mapping)" $ do
          response <- runGetLizardByType "gecko"
          case getResponseBody response of
            GetLizardByTypeResponse200 (LizardGecko gecko) -> do
              geckoDiscriminatorTag gecko `shouldBe` "Gecko"
              geckoHasTail gecko `shouldBe` Just True
            other -> expectationFailure $ "Expected Gecko variant, got: " ++ show other

        it "correctly parses gilaMonster variant using discriminator (no mapping)" $ do
          response <- runGetLizardByType "gilaMonster"
          case getResponseBody response of
            GetLizardByTypeResponse200 (LizardGilaMonster gilaMonster) -> do
              gilaMonsterDiscriminatorTag gilaMonster `shouldBe` "GilaMonster"
              gilaMonsterHasTail gilaMonster `shouldBe` Just False
            other -> expectationFailure $ "Expected GilaMonster variant, got: " ++ show other

        it "returns error for invalid lizard type" $ do
          response <- runGetLizardByType "invalidLizard"
          case getResponseBody response of
            GetLizardByTypeResponseError _ -> pure ()
            GetLizardByTypeResponse400 -> pure ()
            other -> expectationFailure $ "Expected error response, got: " ++ show other
