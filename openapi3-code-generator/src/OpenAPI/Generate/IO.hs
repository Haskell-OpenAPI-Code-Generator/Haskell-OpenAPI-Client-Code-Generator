{-# LANGUAGE DuplicateRecordFields #-}
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
        [ "resolver: lts-18.16",
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
      moduleName = T.unpack $ OAO.settingModuleName settings
   in BF.bimap
        ((outputDirectory </>) . (srcDirectory </>) . (moduleName </>) . (<> ".hs-boot") . foldr1 (</>))
        ( T.unpack
            . T.unlines
            . ( \xs -> case xs of
                  x : xs' -> x : "import Data.Aeson" : "import qualified Data.Aeson as Data.Aeson.Types.Internal" : xs'
                  _ -> xs
              )
            . ( ( \l ->
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
  { moduleFiles :: FilesWithContent,
    cabalFiles :: FilesWithContent,
    stackFiles :: FilesWithContent,
    nixFiles :: FilesWithContent
  }

generateFilesToCreate :: OAT.OpenApiSpecification -> OAO.Settings -> IO OutputFiles
generateFilesToCreate spec settings = do
  let outputDirectory = T.unpack $ OAO.settingOutputDir settings
      moduleName = T.unpack $ OAO.settingModuleName settings
      packageName = T.unpack $ OAO.settingPackageName settings
      env = OAM.createEnvironment settings $ Ref.buildReferenceMap spec
      logMessages = mapM_ (putStrLn . T.unpack) . OAL.filterAndTransformLogs (OAO.settingLogLevel settings)
      showAndReplace = replaceOpenAPI moduleName . show
      ((operationsQ, operationDependencies), logs) = OAM.runGenerator env $ defineOperations moduleName spec
  logMessages logs
  operationModules <- runQ operationsQ
  configurationInfo <- runQ $ defineConfigurationInformation moduleName spec
  let (modelsQ, logsModels) = OAM.runGenerator env $ defineModels moduleName spec operationDependencies
  logMessages logsModels
  modelModules <- fmap (BF.second showAndReplace) <$> runQ modelsQ
  let (securitySchemesQ, logs') = OAM.runGenerator env $ defineSecuritySchemes moduleName spec
  logMessages logs'
  securitySchemes' <- runQ securitySchemesQ
  let modules =
        fmap (BF.bimap ("Operations" :) showAndReplace) operationModules
          <> modelModules
          <> [ (["Configuration"], showAndReplace configurationInfo),
               (["SecuritySchemes"], showAndReplace securitySchemes'),
               (["Common"], replaceOpenAPI moduleName $ replaceVersionNumber $(embedFile "src/OpenAPI/Common.hs"))
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
      hsBootFiles = getHsBootFiles settings modelModules
  pure $
    OutputFiles
      ( BF.second (unlines . lines)
          <$> (mainFile, mainModuleContent) :
        (BF.first ((outputDirectory </>) . (srcDirectory </>) . (moduleName </>) . (<> ".hs") . foldr1 (</>)) <$> modules)
          <> hsBootFiles
      )
      (BF.first (outputDirectory </>) <$> cabalProjectFiles packageName moduleName modulesToExport)
      (BF.first (outputDirectory </>) <$> stackProjectFiles)
      (BF.first (outputDirectory </>) <$> nixProjectFiles packageName)

writeFiles :: OAO.Settings -> OutputFiles -> IO ()
writeFiles settings OutputFiles {..} = do
  let outputDirectory = T.unpack $ OAO.settingOutputDir settings
      moduleName = T.unpack $ OAO.settingModuleName settings
      incremental = OAO.settingIncremental settings
      write = mapM_ $ if incremental then writeFileIncremental else writeFileWithLog
  putStrLn "Remove old output directory"
  unless incremental $
    tryIOError (removeDirectoryRecursive outputDirectory) >> pure ()
  putStrLn "Output directory removed, create missing directories"
  createDirectoryIfMissing True (outputDirectory </> srcDirectory </> moduleName </> "Operations")
  createDirectoryIfMissing True (outputDirectory </> srcDirectory </> moduleName </> "Types")
  putStrLn "Directories created"
  write moduleFiles
  write cabalFiles
  unless (OAO.settingDoNotGenerateStackProject settings) $
    write stackFiles
  when (OAO.settingGenerateNixFiles settings) $
    write nixFiles

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
