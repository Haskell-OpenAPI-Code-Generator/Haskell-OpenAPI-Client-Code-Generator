{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}

module Main where

import Control.Monad
import qualified Data.Bifunctor as BF
import qualified Data.Text as T
import Embed
import Language.Haskell.TH
import OpenAPI.Generate
import qualified OpenAPI.Generate.Doc as Doc
import qualified OpenAPI.Generate.Flags as OAF
import OpenAPI.Generate.Internal.Util
import qualified OpenAPI.Generate.Monad as OAM
import qualified OpenAPI.Generate.Reference as Ref
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
replaceOpenAPI moduleName contents =
  T.unpack
    $ T.replace (T.pack "OpenAPI.Common") (T.pack $ moduleName ++ ".Common")
    $ T.replace (T.pack "OpenAPI.Configuration") (T.pack $ moduleName ++ ".Configuration")
    $ T.pack contents

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
    let env = OAM.createEnvironment opts $ Ref.buildReferenceMap spec
        logMessages = mapM_ (putStrLn . T.unpack) . OAM.transformGeneratorLogs
        moduleName = OAF.optModuleName opts
        showAndReplace = replaceOpenAPI moduleName . show
        (operationsQ, logs) = OAM.runGenerator env $ defineOperations (OAF.optModuleName opts) spec
    logMessages logs
    operationModules <- runQ operationsQ
    configurationInfo <- runQ $ defineConfigurationInformation moduleName spec
    let (modelsQ, logsModels) = OAM.runGenerator env $ defineModels moduleName spec
    logMessages logsModels
    modelModules <- fmap (BF.second showAndReplace) <$> runQ modelsQ
    let (securitySchemesQ, logs') = OAM.runGenerator env $ defineSecuritySchemes moduleName spec
    logMessages logs'
    securitySchemes' <- runQ securitySchemesQ
    let outputDirectory = OAF.optOutputDir opts
        modules =
          fmap (BF.bimap ("Operations" :) showAndReplace) operationModules
            <> modelModules
            <> [ (["Configuration"], showAndReplace configurationInfo),
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
        mainModuleContent = show $ Doc.createModuleHeaderWithReexports moduleName modulesToExport "The main module which exports all functionality."
        hsBootFiles =
          BF.bimap
            ((outputDirectory </>) . (srcDirectory </>) . (moduleName </>) . (<> ".hs-boot") . foldr1 (</>))
            ( T.unpack
                . T.unlines
                . ( \xs -> case xs of
                      x : xs' -> x : "import Data.Aeson" : "import qualified Data.Aeson as Data.Aeson.Types.Internal" : xs'
                      _ -> xs
                  )
                . ( >>=
                      ( \l ->
                          maybe
                            [l]
                            ( \suffix ->
                                [ l,
                                  "instance Show" <> suffix,
                                  "instance Eq" <> suffix,
                                  "instance FromJSON" <> suffix,
                                  "instance ToJSON" <> suffix
                                ]
                            )
                            $ T.stripPrefix "data" l
                      )
                  )
                . fmap (\l -> if T.isPrefixOf "type" l then l else T.takeWhile (/= '=') l)
                . filter (\line -> T.isPrefixOf "data" line || T.isPrefixOf "module" line || T.isPrefixOf "type" line)
                . T.lines
                . T.pack
            )
            <$> filter
              ( \(p, _) -> case p of
                  "Types" : _ : _ -> True
                  _ -> False
              )
              modelModules
        filesToCreate = (mainFile, mainModuleContent) : (BF.first ((outputDirectory </>) . (srcDirectory </>) . (moduleName </>) . (<> ".hs") . foldr1 (</>)) <$> modules) <> hsBootFiles
    if OAF.optDryRun opts
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
        proceed <- permitProceed outputDirectory (OAF.optForce opts)
        if proceed
          then do
            _ <- tryIOError (removeDirectoryRecursive outputDirectory)
            createDirectoryIfMissing True (outputDirectory </> srcDirectory)
            createDirectoryIfMissing True (outputDirectory </> srcDirectory </> moduleName)
            createDirectoryIfMissing True (outputDirectory </> srcDirectory </> moduleName </> "Operations")
            createDirectoryIfMissing True (outputDirectory </> srcDirectory </> moduleName </> "Types")
            mapM_ (uncurry writeFile) filesToCreate
            when (OAF.optGenerateStackProject opts) $
              mapM_
                ( uncurry writeFile
                    . BF.first (outputDirectory </>)
                )
                (stackProjectFiles (OAF.optPackageName opts) moduleName modulesToExport)
            putStrLn "finished"
          else putStrLn "aborted"
  _ -> die "Failed to generate code because no OpenAPI specification was provided. Pass the location of the OpenAPI specification as CLI argument. Run the CLI with --help to see further options."
