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
    let sut = getModelModulesFromModelsWithDependencies "OpenAPI" (Set.fromList ["A", "B", "C", "D", "E", "F", "G"]) False
        t = pure . text
    it "should split string into pieces" $ do
      result <-
        runQ $
          sut
            [ ("A", (t "data A", Set.fromList [])),
              ("B", (t "data B", Set.fromList ["A"])),
              ("C", (t "type C", Set.fromList ["A", "B"])),
              ("D", (t "type D\ndata D", Set.fromList ["C"])),
              ("E", (t "data E", Set.fromList ["E"])),
              ("F", (t "-- XYZ\ntype F", Set.fromList ["G"])),
              ("G", (t "data G", Set.fromList ["F"]))
            ]
      sortBy (\(a, _) (b, _) -> compare a b) (fmap (second show) result)
        `shouldSatisfy` ( \case
                            [ (["TypeAlias"], typeAliasContent),
                              (["Types"], _),
                              (["Types", "A"], aContent),
                              (["Types", "B"], bContent),
                              (["Types", "D"], dContent),
                              (["Types", "E"], eContent),
                              (["Types", "G"], gContent)
                              ] ->
                                and
                                  [ "data A" `isInfixOf` aContent,
                                    "data B" `isInfixOf` bContent,
                                    "type D\ndata D" `isInfixOf` dContent,
                                    "data E" `isInfixOf` eContent,
                                    "data G" `isInfixOf` gContent,
                                    "type C" `isInfixOf` typeAliasContent,
                                    "-- XYZ\ntype F" `isInfixOf` typeAliasContent
                                  ]
                            _ -> False
                        )
