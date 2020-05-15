{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}

{- HLINT ignore "Use list literal" -}
{- HLINT ignore "Redundant bracket" -}
{- HLINT ignore "Redundant do" -}
{- HLINT ignore "Redundant $" -}

module OpenAPI.Generate.OperationTHSpec where

import qualified Data.ByteString.Char8 as B8
import qualified Data.Map as Map
import Data.Maybe
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
  let singleTestTH desc should is = do
        it desc $ do
          expected <- runQ should
          res <- runQ is
          expected `shouldBe` res
      schemaObject = fromJust $ decodeThrow "{}" :: OAS.SchemaObject
      testParameterSchema = OAT.SimpleParameterObjectSchema Nothing False False (OAT.Concrete schemaObject) Nothing Map.empty
      testParameter = OAT.ParameterObject "testName" OAT.QueryParameterObjectLocation Nothing True False True testParameterSchema
      testParameterOtherName = OAT.ParameterObject "testName2" OAT.QueryParameterObjectLocation Nothing True False True testParameterSchema
      testTHName = mkName "myTestName"
      testTHE = [|B8.unpack (HT.urlEncode True $ B8.pack $ OC.stringifyModel $(varE testTHName))|]
      monadName = mkName "m"
      securitySchemeName = mkName "s"
   in do
        describe "generateQueryParams" $ do
          singleTestTH
            "should generate empty list"
            (generateQueryParams [])
            [|[]|]
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
            (generateParameterizedRequestPath [(mkName "myTestName", testParameter)] (T.pack "/my/path/"))
            [|"/my/path/"|]
          singleTestTH
            "should replace one occurences at the end"
            (generateParameterizedRequestPath [(mkName "myTestName", testParameter)] (T.pack "/my/path/{testName}"))
            [|"/my/path/" ++ $(testTHE) ++ ""|]
          singleTestTH
            "should replace one occurences at the end"
            (generateParameterizedRequestPath [(mkName "myTestName", testParameter)] (T.pack "/my/path/{testName}/"))
            [|"/my/path/" ++ $(testTHE) ++ "/"|]
          singleTestTH
            "should replace one occurences at the beginning"
            (generateParameterizedRequestPath [(mkName "myTestName", testParameter)] (T.pack "{testName}/my/path/"))
            [|"" ++ $(testTHE) ++ "/my/path/"|]
          singleTestTH
            "should replace one occurences at the beginning"
            (generateParameterizedRequestPath [(mkName "myTestName", testParameter)] (T.pack "/{testName}/my/path/"))
            [|"/" ++ $(testTHE) ++ "/my/path/"|]
          singleTestTH
            "should replace one occurences in the middle"
            (generateParameterizedRequestPath [(mkName "myTestName", testParameter)] (T.pack "/another/test/{testName}/my/path/"))
            [|"/another/test/" ++ $(testTHE) ++ "/my/path/"|]
          singleTestTH
            "should ignore names not given"
            (generateParameterizedRequestPath [] (T.pack "/another/test/{testName}/my/path/"))
            [|"/another/test/{testName}/my/path/"|]
          singleTestTH
            "should replace two occurences"
            ( generateParameterizedRequestPath
                [(mkName "myTestName", testParameter), (mkName "myTestName2", testParameterOtherName)]
                (T.pack "/{testName2}/my//test/{testName}/my/path/")
            )
            [|("/" ++ B8.unpack (HT.urlEncode True $ B8.pack $ OC.stringifyModel $(varE $ mkName "myTestName2")) ++ "/my//test/") ++ $(testTHE) ++ "/my/path/"|]
          singleTestTH
            "should replace one variable twice in the path"
            ( generateParameterizedRequestPath
                [(mkName "myTestName", testParameter)]
                (T.pack "/{testName}/my//test/{testName}/my/path/")
            )
            [|"/" ++ $(testTHE) ++ "/my//test/" ++ $(testTHE) ++ "/my/path/"|]
        describe "getParametersTypeForSignature" $ do
          let responseTypeName = mkName "Test"
              responseType = [t|$(varT monadName) (Either HS.HttpException (HS.Response $(varT responseTypeName)))|]
          singleTestTH
            "no parameters"
            (getParametersTypeForSignature [] responseTypeName monadName securitySchemeName)
            [t|OC.Configuration $(varT securitySchemeName) -> $(responseType)|]
          singleTestTH
            "One parameters"
            (getParametersTypeForSignature [conT ''Int] responseTypeName monadName securitySchemeName)
            [t|OC.Configuration $(varT securitySchemeName) -> Int -> $(responseType)|]
          singleTestTH
            "Optional parameters"
            (getParametersTypeForSignature [[t|Maybe Int|]] responseTypeName monadName securitySchemeName)
            [t|OC.Configuration $(varT securitySchemeName) -> Maybe Int -> $(responseType)|]
          singleTestTH
            "Two parameters"
            (getParametersTypeForSignature [conT ''Int, conT ''String] responseTypeName monadName securitySchemeName)
            [t|OC.Configuration $(varT securitySchemeName) -> Int -> String -> $(responseType)|]
