{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE OverloadedStrings #-}

-- | Functionality to Generate Haskell Code out of an OpenAPI definition File
module OpenAPI.Generate where

import Data.Text (Text)
import qualified Data.Text as T
import Data.Yaml
import qualified OpenAPI.Generate.IO as OAI
import qualified OpenAPI.Generate.OptParse as OAO
import qualified OpenAPI.Generate.Types as OAT
import Options.Applicative
import System.Exit

-- | Decodes an OpenAPI File
decodeOpenApi :: Text -> IO OAT.OpenApiSpecification
decodeOpenApi fileName = do
  res <- decodeFileEither $ T.unpack fileName
  case res of
    Left exc -> die $ "Could not parse OpenAPI specification '" <> T.unpack fileName <> "': " <> show exc
    Right o -> pure o

-- | Run the generator as CLI
runGenerator :: IO ()
runGenerator =
  let opts =
        info (OAO.parseOptions <**> helper) $
          mconcat
            [ fullDesc,
              progDesc "This tool can be used to generate Haskell code from OpenAPI 3 specifications. For more information see https://github.com/Haskell-OpenAPI-Code-Generator/Haskell-OpenAPI-Client-Code-Generator.",
              header "Generate Haskell code from OpenAPI 3 specifications"
            ]
   in execParser opts >>= \options -> do
        let flags = OAO.optFlags options
        spec <- decodeOpenApi $ OAO.optSpecification options
        (moduleFiles, projectFiles) <- OAI.generateFilesToCreate spec options
        if OAO.flagDryRun flags
          then
            mapM_
              ( \(file, content) -> do
                  putStrLn $ "File: " <> file
                  putStrLn "---"
                  putStrLn content
                  putStrLn "---\n\n"
              )
              moduleFiles
          else do
            proceed <- OAI.permitProceed flags
            if proceed
              then do
                OAI.writeFiles flags moduleFiles projectFiles
                putStrLn "finished"
              else putStrLn "aborted"
