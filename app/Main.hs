{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE TemplateHaskell #-}

module Main where

import Control.Monad
import Data.Bifunctor
import qualified Data.Text as T
import Embed
import Language.Haskell.TH
import OpenAPI.Generate
import OpenAPI.Generate.Doc
import OpenAPI.Generate.Flags
import OpenAPI.Generate.Internal.Util
import qualified OpenAPI.Generate.Monad as OAM
import OpenAPI.Generate.Reference
import Options
import System.Directory
import System.Exit
import System.FilePath
import System.IO.Error

srcDirectory :: FilePath
srcDirectory = "src"

-- | Creates files from mostly static data
stackProjectFiles ::
  -- | Name of the cabal project
  String ->
  -- | Name of the module
  String ->
  -- | Modules to export
  [String] ->
  [(FilePath, String)]
stackProjectFiles packageName moduleName modulesToExport =
  [ ( packageName ++ ".cabal",
      unlines $
        [ "cabal-version: 1.12",
          "",
          "name:           " ++ packageName,
          "version:        0.1.0.0",
          "build-type:     Simple",
          "",
          "library",
          "  exposed-modules:",
          "      " ++ moduleName
        ]
          <> fmap ("      " <>) modulesToExport
          <> [ "  hs-source-dirs:",
               "      src",
               "  build-depends:",
               "      base >=4.7 && <5",
               "    , text",
               "    , ghc-prim",
               "    , http-conduit",
               "    , http-client",
               "    , http-types",
               "    , bytestring",
               "    , lens",
               "    , aeson",
               "    , unordered-containers",
               "    , vector",
               "    , scientific",
               "    , time",
               "    , mtl",
               "    , transformers",
               "  default-language: Haskell2010"
             ]
    ),
    ( "stack.yaml",
      unlines
        [ "resolver: lts-15.3",
          "packages:",
          "- ."
        ]
    )
  ]

replaceOpenAPI :: String -> String -> String
replaceOpenAPI moduleName contents = T.unpack $ T.replace (T.pack "OpenAPI.Common") (T.pack $ moduleName ++ ".Common") (T.pack contents)

permitProceed :: FilePath -> Bool -> IO Bool
permitProceed outputDirectory force = do
  outputDirectoryExists <- doesPathExist outputDirectory
  if outputDirectoryExists
    then
      if force
        then do
          putStrLn "output directory already exists and will be overwritten"
          pure True
        else do
          putStrLn "The output directory "
          putStrLn ""
          putStrLn $ "      " ++ show outputDirectory
          putStrLn ""
          putStrLn "already exists. overwrite? Y/N"
          putStrLn ""
          answer <- getLine
          pure $ (answer == "Y") || (answer == "y")
    else do
      putStrLn "output directory will be created"
      pure True

main :: IO ()
main = runCommand $ \opts args -> case args of
  [path] -> do
    spec <- decodeOpenApi path
    let env = OAM.createEnvironment opts $ buildReferenceMap spec
        logMessages = mapM_ (putStrLn . T.unpack) . OAM.transformGeneratorLogs
        moduleName = optModuleName opts
        (operationsQ, logs) = OAM.runGenerator env $ defineOperations (optModuleName opts) spec
    logMessages logs
    operationModules <- runQ operationsQ
    serverInfo <- runQ $ defineServerInformation moduleName spec
    let (modelsQ, logsModels) = OAM.runGenerator env $ defineModelsFromSpec moduleName spec
    logMessages logsModels
    modelModules <- runQ modelsQ
    let (securitySchemesQ, logs') = OAM.runGenerator env $ defineSecuritySchemes moduleName spec
    logMessages logs'
    securitySchemes' <- runQ securitySchemesQ
    let outputDirectory = optOutputDir opts
        showAndReplace = replaceOpenAPI moduleName . show
        modules =
          fmap (bimap ("Operations" :) showAndReplace) operationModules
            <> fmap (second showAndReplace) modelModules
            <> [ (["Configuration"], showAndReplace serverInfo),
                 (["SecuritySchemes"], showAndReplace securitySchemes'),
                 (["Common"], replaceOpenAPI moduleName $(embedFile "src/OpenAPI/Common.hs"))
               ]
        modulesToExport =
          fmap
            ( (moduleName <>) . ("." <>)
                . joinWithPoint
                . fst
            )
            modules
        mainFile = outputDirectory </> srcDirectory </> (moduleName ++ ".hs")
        mainModuleContent = show $ createModuleHeaderWithReexports moduleName modulesToExport
        filesToCreate = (mainFile, mainModuleContent) : (first ((outputDirectory </>) . (srcDirectory </>) . (moduleName </>) . (<> ".hs") . foldr1 (</>)) <$> modules)
    if optDryRun opts
      then
        mapM_
          ( \(file, content) -> do
              putStrLn $ "File: " <> file
              putStrLn "---"
              putStrLn content
              putStrLn "---\n\n"
          )
          filesToCreate
      else do
        proceed <- permitProceed outputDirectory (optForce opts)
        if proceed
          then do
            _ <- tryIOError (removeDirectoryRecursive outputDirectory)
            createDirectoryIfMissing True (outputDirectory </> srcDirectory)
            createDirectoryIfMissing True (outputDirectory </> srcDirectory </> moduleName)
            createDirectoryIfMissing True (outputDirectory </> srcDirectory </> moduleName </> "Operations")
            createDirectoryIfMissing True (outputDirectory </> srcDirectory </> moduleName </> "Types")
            mapM_ (uncurry writeFile) filesToCreate
            when (optGenerateStackProject opts) $
              mapM_
                ( uncurry writeFile
                    . first (outputDirectory </>)
                )
                (stackProjectFiles (optPackageName opts) moduleName modulesToExport)
            putStrLn "finished"
          else putStrLn "aborted"
  _ -> die "Failed to generate code because no OpenAPI specification was provided. Pass the location of the OpenAPI specification as CLI argument. Run the CLI with --help to see further options."
