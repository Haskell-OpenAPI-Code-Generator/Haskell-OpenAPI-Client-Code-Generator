{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

-- | Functionality to Generate Haskell Code out of an OpenAPI definition File
module OpenAPI.Generate where

import Data.Text (Text)
import qualified Data.Text as T
import Data.Yaml
import qualified OpenAPI.Generate.IO as OAI
import qualified OpenAPI.Generate.OptParse as OAO
import qualified OpenAPI.Generate.Types as OAT
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
runGenerator = do
  settings <- OAO.getSettings
  spec <- decodeOpenApi $ OAO.settingOpenApiSpecification settings
  outFiles@OAI.OutputFiles {..} <- OAI.generateFilesToCreate spec settings
  if OAO.settingDryRun settings
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
      proceed <- OAI.permitProceed settings
      if proceed
        then do
          OAI.writeFiles settings outFiles
          putStrLn "finished"
        else putStrLn "aborted"
