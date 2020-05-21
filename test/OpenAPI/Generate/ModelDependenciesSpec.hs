{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}

module OpenAPI.Generate.ModelDependenciesSpec where

import Data.Bifunctor
import Data.List
import qualified Data.Set as Set
import Language.Haskell.TH
import Language.Haskell.TH.PprLib hiding ((<>))
import OpenAPI.Generate.ModelDependencies
import Test.Hspec

spec :: Spec
spec =
  describe "getModelModulesFromModelsWithDependencies" $ do
    let sut = getModelModulesFromModelsWithDependencies "OpenAPI"
        t = pure . text
    it "should split string into pieces" $ do
      result <-
        runQ $
          sut
            [ ("A", (t "aToken", Set.fromList [])),
              ("B", (t "bToken", Set.fromList ["A"])),
              ("C", (t "cToken", Set.fromList ["A", "B"])),
              ("D", (t "dToken", Set.fromList ["C"])),
              ("E", (t "eToken", Set.fromList ["E"])),
              ("F", (t "fToken", Set.fromList ["G"])),
              ("G", (t "gToken", Set.fromList ["F"]))
            ]
      sortBy (\(a, _) (b, _) -> compare a b) (fmap (second show) result)
        `shouldSatisfy` ( \case
                            [ (["CyclicTypes"], cyclicContent),
                              (["Types"], _),
                              (["Types", "A"], aContent),
                              (["Types", "B"], bContent),
                              (["Types", "C"], cContent),
                              (["Types", "D"], dContent)
                              ] ->
                                and
                                  [ "aToken" `isInfixOf` aContent,
                                    "bToken" `isInfixOf` bContent,
                                    "cToken" `isInfixOf` cContent,
                                    "dToken" `isInfixOf` dContent,
                                    "eToken" `isInfixOf` cyclicContent,
                                    "fToken" `isInfixOf` cyclicContent,
                                    "gToken" `isInfixOf` cyclicContent
                                  ]
                            _ -> False
                        )
