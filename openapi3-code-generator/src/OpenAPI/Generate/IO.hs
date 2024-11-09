{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell #-}

module OpenAPI.Generate.IO where

import Control.Exception
import Control.Monad
import qualified Data.Bifunctor as BF
import qualified Data.Text as T
import Data.Version (showVersion)
import Language.Haskell.TH
import qualified OpenAPI.Generate.Doc as Doc
import OpenAPI.Generate.Internal.Embed
import OpenAPI.Generate.Internal.Util
import qualified OpenAPI.Generate.Log as OAL
import OpenAPI.Generate.Main
import qualified OpenAPI.Generate.Monad as OAM
import qualified OpenAPI.Generate.OptParse as OAO
import qualified OpenAPI.Generate.Reference as Ref
import qualified OpenAPI.Generate.Types as OAT
import Paths_openapi3_code_generator (version)
import System.Directory
import System.FilePath
import System.IO.Error

type FileWithContent = (FilePath, String)

type FilesWithContent = [FileWithContent]

srcDirectory :: FilePath
srcDirectory = "src"

-- | Creates files from mostly static data
cabalProjectFiles ::
  -- | Name of the cabal project
  String ->
  -- | Name of the module
  String ->
  -- | Modules to export
  [String] ->
  FilesWithContent
cabalProjectFiles packageName moduleName modulesToExport =
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
    )
  ]

-- | Creates stack support files
stackProjectFiles ::
  FilesWithContent
stackProjectFiles =
  [ ( "stack.yaml",
      unlines
        [ "resolver: lts-21.22",
          "packages:",
          "- ."
        ]
    )
  ]

-- | Creates nix support files
nixProjectFiles ::
  -- | Name of the cabal project
  String ->
  FilesWithContent
nixProjectFiles packageName =
  [ ( "default.nix",
      unlines
        [ "{ pkgs ? import <nixpkgs> {} }:",
          "let",
          "  src = pkgs.nix-gitignore.gitignoreSource [ ] ./.;",
          "in",
          "  pkgs.haskellPackages.callCabal2nix \"" ++ packageName ++ "\" ./. { }"
        ]
    ),
    ( "shell.nix",
      unlines
        [ "{ pkgs ? import <nixpkgs> {} }:",
          "(import ./default.nix { inherit pkgs; }).env"
        ]
    )
  ]

replaceOpenAPI :: String -> String -> String
replaceOpenAPI moduleName contents =
  T.unpack $
    T.replace "OpenAPI.Common" (T.pack $ moduleName ++ ".Common") $
      T.replace "OpenAPI.Configuration" (T.pack $ moduleName ++ ".Configuration") $
        T.pack contents

replaceVersionNumber :: String -> String
replaceVersionNumber contents =
  T.unpack $
    T.replace "VERSION_TO_REPLACE" (T.pack $ showVersion version) $
      T.pack contents

permitProceed :: OAO.Settings -> IO Bool
permitProceed settings = do
  let outputDirectory = T.unpack $ OAO.settingOutputDir settings
  outputDirectoryExists <- doesPathExist outputDirectory
  if outputDirectoryExists
    then
      if OAO.settingForce settings || OAO.settingIncremental settings
        then do
          putStrLn "Output directory already exists and will be overwritten"
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
      putStrLn "Output directory will be created"
      pure True

getHsBootFiles :: OAO.Settings -> [([String], String)] -> FilesWithContent
getHsBootFiles settings modelModules =
  let outputDirectory = T.unpack $ OAO.settingOutputDir settings
      moduleName = OAO.settingModuleName settings
      moduleNameStr = OAO.getModuleName moduleName
      modulePathInfo = OAO.mkModulePathInfo moduleName
   in BF.bimap
        ((\suffix -> outputDirectory </> srcDirectory </> OAO.getModuleInfoPath modulePathInfo (Just suffix) ".hs-boot") . foldr1 (</>))
        ( T.unpack
            . T.unlines
            . ( \xs -> case xs of
                  x : xs' ->
                    x
                      : "import qualified Data.Aeson"
                      : "import qualified " <> T.pack moduleNameStr <> ".Common"
                      : xs'
                  _ -> xs
              )
            . ( ( \l ->
                    maybe
                      [l]
                      ( \suffix ->
                          [ l,
                            "instance Show" <> suffix,
                            "instance Eq" <> suffix,
                            "instance Data.Aeson.FromJSON" <> suffix,
                            "instance Data.Aeson.ToJSON" <> suffix
                          ]
                      )
                      $ T.stripPrefix "data" l
                )
                  <=< ( fmap (\l -> if T.isPrefixOf "type" l then l else T.strip (T.takeWhile (/= '=') l))
                          . filter (\line -> T.isPrefixOf "data" line || T.isPrefixOf "module" line || T.isPrefixOf "type" line)
                          . T.lines
                          . T.pack
                      )
              )
        )
        <$> filter
          ( \(p, _) -> case p of
              "Types" : _ : _ -> True
              _ -> False
          )
          modelModules

data OutputFiles = OutputFiles
  { outputFilesModuleFiles :: FilesWithContent,
    outputFilesCabalFiles :: FilesWithContent,
    outputFilesStackFiles :: FilesWithContent,
    outputFilesNixFiles :: FilesWithContent
  }

generateFilesToCreate :: OAT.OpenApiSpecification -> OAO.Settings -> IO OutputFiles
generateFilesToCreate spec settings = do
  let outputDirectory = T.unpack $ OAO.settingOutputDir settings
      moduleName = OAO.settingModuleName settings
      modulePathInfo = OAO.mkModulePathInfo moduleName
      moduleNameStr = OAO.getModuleName moduleName
      packageName = T.unpack $ OAO.settingPackageName settings
      env = OAM.createEnvironment settings $ Ref.buildReferenceMap spec
      logMessages = mapM_ (putStrLn . T.unpack) . OAL.filterAndTransformLogs (OAO.settingLogLevel settings)
      showAndReplace = replaceOpenAPI moduleNameStr . show
      ((operationsQ, operationDependencies), logs) = OAM.runGenerator env $ defineOperations moduleNameStr spec
  logMessages logs
  operationModules <- runQ operationsQ
  configurationInfo <- runQ $ defineConfigurationInformation moduleNameStr spec
  let (modelsQ, logsModels) = OAM.runGenerator env $ defineModels moduleNameStr spec operationDependencies
  logMessages logsModels
  modelModules <- fmap (BF.second showAndReplace) <$> runQ modelsQ
  let (securitySchemesQ, logs') = OAM.runGenerator env $ defineSecuritySchemes moduleNameStr spec
  logMessages logs'
  securitySchemes' <- runQ securitySchemesQ
  let modules =
        fmap (BF.bimap ("Operations" :) showAndReplace) operationModules
          <> modelModules
          <> [ (["Configuration"], showAndReplace configurationInfo),
               (["SecuritySchemes"], showAndReplace securitySchemes'),
               (["Common"], replaceOpenAPI moduleNameStr $ replaceVersionNumber $(embedFile "src/OpenAPI/Common.hs"))
             ]
      modulesToExport =
        fmap
          ( (moduleNameStr <>)
              . ("." <>)
              . joinWithPoint
              . fst
          )
          modules
      mainFile = outputDirectory </> srcDirectory </> OAO.getModuleInfoPath modulePathInfo Nothing ".hs"
      mainModuleContent = show $ Doc.createModuleHeaderWithReexports moduleNameStr modulesToExport "The main module which exports all functionality."
      hsBootFiles = getHsBootFiles settings modelModules
  pure $
    OutputFiles
      ( BF.second (unlines . lines)
          <$> (mainFile, mainModuleContent)
            : (BF.first ((\suffix -> outputDirectory </> srcDirectory </> OAO.getModuleInfoPath modulePathInfo (Just suffix) ".hs") . foldr1 (</>)) <$> modules)
              <> hsBootFiles
      )
      (BF.first (outputDirectory </>) <$> cabalProjectFiles packageName moduleNameStr modulesToExport)
      (BF.first (outputDirectory </>) <$> stackProjectFiles)
      (BF.first (outputDirectory </>) <$> nixProjectFiles packageName)

writeFiles :: OAO.Settings -> OutputFiles -> IO ()
writeFiles settings OutputFiles {..} = do
  let outputDirectory = T.unpack $ OAO.settingOutputDir settings
      modulePathInfo = OAO.mkModulePathInfo $ OAO.settingModuleName settings
      moduleDir = OAO.getModuleInfoDir modulePathInfo
      incremental = OAO.settingIncremental settings
      write = mapM_ $ if incremental then writeFileIncremental else writeFileWithLog
  putStrLn "Remove old output directory"
  unless incremental $
    void $
      tryIOError (removeDirectoryRecursive outputDirectory)
  putStrLn "Output directory removed, create missing directories"
  createDirectoryIfMissing True (outputDirectory </> srcDirectory </> moduleDir </> "Operations")
  createDirectoryIfMissing True (outputDirectory </> srcDirectory </> moduleDir </> "Types")
  putStrLn "Directories created"
  write outputFilesModuleFiles
  write outputFilesCabalFiles
  unless (OAO.settingDoNotGenerateStackProject settings) $
    write outputFilesStackFiles
  when (OAO.settingGenerateNixFiles settings) $
    write outputFilesNixFiles

writeFileWithLog :: FileWithContent -> IO ()
writeFileWithLog (filePath, content) = do
  putStrLn $ "Write file to path: " <> filePath
  writeFile filePath content

writeFileIncremental :: FileWithContent -> IO ()
writeFileIncremental (filePath, content) = do
  oldContent <-
    (Just <$> readFile filePath)
      `catch` ( \(_ :: IOException) -> do
                  writeFile filePath content
                  pure Nothing
              )
  when (maybe 0 length oldContent > 0 && oldContent /= Just content) $
    writeFile filePath content
