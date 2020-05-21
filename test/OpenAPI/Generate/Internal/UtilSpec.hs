module OpenAPI.Generate.Internal.UtilSpec where

import OpenAPI.Generate.Internal.Util
import Test.Hspec
import Test.Validity

spec :: Spec
spec =
  describe "splitOn" $ do
    it "should split string into pieces" $
      splitOn 'a' "abcabca" `shouldBe` ["", "bc", "bc", ""]
    it "should have one split more than elements to split on"
      $ forAllValid
      $ \(x, list) ->
        length (splitOn (x :: Char) list)
          == length (filter (== x) list) + 1
