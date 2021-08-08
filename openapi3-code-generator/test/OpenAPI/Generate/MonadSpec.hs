{-# LANGUAGE OverloadedStrings #-}

module OpenAPI.Generate.MonadSpec where

import Control.Monad.Reader
import Data.Bifunctor
import Data.GenValidity.Text ()
import Data.List
import qualified Data.Map as Map
import qualified Data.Text as T
import Data.Validity.Text ()
import qualified OpenAPI.Generate.Log as OAL
import OpenAPI.Generate.Monad
import qualified OpenAPI.Generate.OptParse as OAO
import OpenAPI.Generate.Reference
import OpenAPI.Generate.Types as OAT
import Test.Hspec
import Test.Validity

spec :: Spec
spec = do
  defaultSettings <- runIO OAO.getSettings
  let run = runGenerator (createEnvironment defaultSettings Map.empty)
  describe "nested" $ do
    it "should nest path correct with simple example" $ do
      let (currentPath', _) = run $ nested "a" $ nested "b" $ nested "c" $ asks currentPath
      currentPath' `shouldBe` ["a", "b", "c"]
    it "should nest path correct with property" $
      forAllValid $ \list -> do
        let (currentPath', _) = run $ foldr nested (asks currentPath) list
        currentPath' `shouldBe` list
    it "should save path with logs" $
      let (_, logs) = run $
            nested "a" $ do
              logInfo "1"
              nested "b" $ do
                logInfo "2"
                nested "c" $ logInfo "3"
                logInfo "4"
              logInfo "5"
       in fmap (\l -> (OAL.logEntryPath l, OAL.logEntryMessage l)) logs
            `shouldBe` [ (["a"], "1"),
                         (["a", "b"], "2"),
                         (["a", "b", "c"], "3"),
                         (["a", "b"], "4"),
                         (["a"], "5")
                       ]
  describe "logs" $ do
    let msgAndLogToTupleList list logs = bimap T.unpack T.unpack <$> zip list (OAL.transformLogs logs)
        logContainsMsgPredicate = all $ uncurry isInfixOf
    it "should contain all logged info messages" $
      forAllValid $ \list -> do
        let (_, logs) = run $ foldr ((>>) . logInfo) (pure ()) list
        shouldSatisfy (msgAndLogToTupleList list logs) logContainsMsgPredicate
    it "should contain all logged warn messages" $
      forAllValid $ \list -> do
        let (_, logs) = run $ foldr ((>>) . logWarning) (pure ()) list
        shouldSatisfy (msgAndLogToTupleList list logs) logContainsMsgPredicate
    it "should contain all logged error messages" $
      forAllValid $ \list -> do
        let (_, logs) = run $ foldr ((>>) . logError) (pure ()) list
        shouldSatisfy (msgAndLogToTupleList list logs) logContainsMsgPredicate
  describe "reference lookup" $ do
    let e = OAT.ExampleObject (Just "foo") Nothing Nothing (Just "http://example.com")
        b = OAT.RequestBodyObject Map.empty Nothing False
        runWithEnv =
          runGenerator $
            createEnvironment defaultSettings $
              Map.fromList
                [ ("#/components/examples/example1", ExampleReference e),
                  ("#/components/requestBodies/body1", RequestBodyReference b)
                ]
    it "should find existing reference to example" $ do
      let (maybeExample, _) = runWithEnv $ getExampleReferenceM "#/components/examples/example1"
      maybeExample `shouldBe` Just e
    it "should find existing reference to request body" $ do
      let (maybeBody, _) = runWithEnv $ getRequestBodyReferenceM "#/components/requestBodies/body1"
      maybeBody `shouldBe` Just b
    it "should not find existing reference with wrong type" $ do
      let (maybeExample, _) = runWithEnv $ getExampleReferenceM "#/components/requestBodies/body1"
      maybeExample `shouldBe` Nothing
    it "should not find non-existing reference" $ do
      let (maybeExample, _) = runWithEnv $ getExampleReferenceM "#/components/examples/example99"
      maybeExample `shouldBe` Nothing
