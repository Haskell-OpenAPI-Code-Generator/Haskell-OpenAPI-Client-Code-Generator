{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE TemplateHaskell #-}

module OpenAPI.Generate.OperationTHSpec where

import qualified Data.ByteString.Char8 as B8
import qualified Data.Map as Map
import qualified Data.Maybe as Maybe
import Data.Text as T
import Data.Yaml
import Language.Haskell.TH
import qualified Network.HTTP.Simple as HS
import qualified Network.HTTP.Types as HT
import qualified OpenAPI.Common as OC
import OpenAPI.Generate.Internal.Operation
import OpenAPI.Generate.Types as OAT
import OpenAPI.Generate.Types.Schema as OAS
import Test.Hspec

spec :: Spec
spec =
  let singleTestTH desc should is =
        it desc $ do
          expected <- runQ should
          res <- runQ is
          expected `shouldBe` res
      schemaObject = Maybe.fromJust $ decodeThrow "{}" :: OAS.SchemaObject
      testParameterSchema = OAT.SimpleParameterObjectSchema $ OAT.SimpleParameterSchema Nothing False False (OAT.Concrete schemaObject) Nothing Map.empty
      testParameter = OAT.ParameterObject "testName" OAT.QueryParameterObjectLocation Nothing True False True testParameterSchema
      testParameterOtherName = OAT.ParameterObject "testName2" OAT.QueryParameterObjectLocation Nothing True False True testParameterSchema
      testTHName = varE $ mkName "myTestName"
      testTHE = [|B8.unpack (HT.urlEncode True $ B8.pack $ OC.stringifyModel $testTHName)|]
      monadName = mkName "m"
   in do
        describe "generateQueryParams" $
          singleTestTH
            "should generate empty list"
            (generateQueryParams [])
            [|mempty|]
        describe "generateParameterizedRequestPath" $ do
          singleTestTH
            "should not change empty path without arguments"
            (generateParameterizedRequestPath [] (T.pack ""))
            [|""|]
          singleTestTH
            "should not change path without arguments"
            (generateParameterizedRequestPath [] (T.pack "/my/path/"))
            [|"/my/path/"|]
          singleTestTH
            "should ignore params not names"
            (generateParameterizedRequestPath [(testTHName, testParameter)] (T.pack "/my/path/"))
            [|"/my/path/"|]
          singleTestTH
            "should replace one occurences at the end"
            (generateParameterizedRequestPath [(testTHName, testParameter)] (T.pack "/my/path/{testName}"))
            [|"/my/path/" ++ $(testTHE) ++ ""|]
          singleTestTH
            "should replace one occurences at the end"
            (generateParameterizedRequestPath [(testTHName, testParameter)] (T.pack "/my/path/{testName}/"))
            [|"/my/path/" ++ $(testTHE) ++ "/"|]
          singleTestTH
            "should replace one occurences at the beginning"
            (generateParameterizedRequestPath [(testTHName, testParameter)] (T.pack "{testName}/my/path/"))
            [|"" ++ $(testTHE) ++ "/my/path/"|]
          singleTestTH
            "should replace one occurences at the beginning"
            (generateParameterizedRequestPath [(testTHName, testParameter)] (T.pack "/{testName}/my/path/"))
            [|"/" ++ $(testTHE) ++ "/my/path/"|]
          singleTestTH
            "should replace one occurences in the middle"
            (generateParameterizedRequestPath [(testTHName, testParameter)] (T.pack "/another/test/{testName}/my/path/"))
            [|"/another/test/" ++ $(testTHE) ++ "/my/path/"|]
          singleTestTH
            "should ignore names not given"
            (generateParameterizedRequestPath [] (T.pack "/another/test/{testName}/my/path/"))
            [|"/another/test/{testName}/my/path/"|]
          singleTestTH
            "should replace two occurences"
            ( generateParameterizedRequestPath
                [(testTHName, testParameter), (varE $ mkName "myTestName2", testParameterOtherName)]
                (T.pack "/{testName2}/my//test/{testName}/my/path/")
            )
            [|("/" ++ B8.unpack (HT.urlEncode True $ B8.pack $ OC.stringifyModel $(varE $ mkName "myTestName2")) ++ "/my//test/") ++ $(testTHE) ++ "/my/path/"|]
          singleTestTH
            "should replace one variable twice in the path"
            ( generateParameterizedRequestPath
                [(testTHName, testParameter)]
                (T.pack "/{testName}/my//test/{testName}/my/path/")
            )
            [|"/" ++ $(testTHE) ++ "/my//test/" ++ $(testTHE) ++ "/my/path/"|]
        describe "getParametersTypeForSignature" $ do
          let responseTypeName = mkName "Test"
              responseType = [t|$(varT monadName) (HS.Response $(varT responseTypeName))|]
          singleTestTH
            "no parameters"
            (getParametersTypeForSignature [] responseTypeName monadName)
            [t|OC.Configuration -> $(responseType)|]
          singleTestTH
            "One parameters"
            (getParametersTypeForSignature [conT ''Int] responseTypeName monadName)
            [t|OC.Configuration -> Int -> $(responseType)|]
          singleTestTH
            "Optional parameters"
            (getParametersTypeForSignature [[t|Maybe Int|]] responseTypeName monadName)
            [t|OC.Configuration -> Maybe Int -> $(responseType)|]
          singleTestTH
            "Two parameters"
            (getParametersTypeForSignature [conT ''Int, conT ''T.Text] responseTypeName monadName)
            [t|OC.Configuration -> Int -> T.Text -> $(responseType)|]
