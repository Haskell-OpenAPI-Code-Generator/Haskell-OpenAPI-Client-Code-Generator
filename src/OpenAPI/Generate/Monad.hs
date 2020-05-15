{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

-- | This module contains the 'Generator' monad and functions which deal with this monad.
-- In addition this module contains the means for logging and resolving references since they are
-- closely linked to the 'Generator' monad.
module OpenAPI.Generate.Monad where

import Control.Monad
import Control.Monad.Reader
import Control.Monad.Writer
import Data.List
import Data.Text (Text)
import qualified Data.Text as T
import OpenAPI.Generate.Flags
import OpenAPI.Generate.Reference
import OpenAPI.Generate.Types
import OpenAPI.Generate.Types.Schema

-- | The reader environment of the 'Generator' monad
--
-- The 'currentPath' is updated using the 'nested' function to track the current position within the specification.
-- This is used to produce tracable log messages.
-- The 'references' map is a lookup table for references within the OpenAPI specification.
data GeneratorEnvironment
  = GeneratorEnvironment
      { currentPath :: [Text],
        references :: ReferenceMap,
        flags :: Flags
      }
  deriving (Show, Eq)

-- | Data type representing the log severities
data GeneratorLogSeverity = ErrorSeverity | WarningSeverity | InfoSeverity
  deriving (Show, Eq)

-- | A log entry containing the location within the OpenAPI specification where the message was produced, a severity and the actual message.
data GeneratorLogEntry
  = GeneratorLogEntry
      { path :: [Text],
        severity :: GeneratorLogSeverity,
        message :: Text
      }
  deriving (Show, Eq)

-- | The type contained in the writer of the 'Generator' used to collect log entries
type GeneratorLogs = [GeneratorLogEntry]

-- | The 'Generator' monad is used to pass a 'ReaderT' environment to functions in need of resolving references
-- and collects log messages.
newtype Generator a = Generator {unGenerator :: WriterT GeneratorLogs (Reader GeneratorEnvironment) a}
  deriving (Functor, Applicative, Monad, MonadReader GeneratorEnvironment, MonadWriter GeneratorLogs)

-- | Runs the generator monad within a provided environment.
runGenerator :: GeneratorEnvironment -> Generator a -> (a, GeneratorLogs)
runGenerator e (Generator g) = runReader (runWriterT g) e

-- | Create an environment based on a 'ReferenceMap' and 'Flags'
createEnvironment :: Flags -> ReferenceMap -> GeneratorEnvironment
createEnvironment flags references =
  GeneratorEnvironment
    { currentPath = [],
      references = references,
      flags = flags
    }

-- | Writes a log message to a 'Generator' monad
logMessage :: GeneratorLogSeverity -> Text -> Generator ()
logMessage severity message = do
  path' <- asks currentPath
  tell [GeneratorLogEntry {path = path', severity = severity, message = message}]

-- | Writes an error to a 'Generator' monad
logError :: Text -> Generator ()
logError = logMessage ErrorSeverity

-- | Writes a warning to a 'Generator' monad
logWarning :: Text -> Generator ()
logWarning = logMessage WarningSeverity

-- | Writes an info to a 'Generator' monad
logInfo :: Text -> Generator ()
logInfo = logMessage InfoSeverity

-- | Transforms a log returned from 'runGenerator' to a list of 'Text' values for easier printing.
transformGeneratorLogs :: GeneratorLogs -> [Text]
transformGeneratorLogs =
  fmap
    ( \GeneratorLogEntry {..} ->
        transformSeverity severity <> " (" <> transformPath path <> "): " <> message
    )

-- | Transforms the severity to a 'Text' representation
transformSeverity :: GeneratorLogSeverity -> Text
transformSeverity ErrorSeverity = "ERROR"
transformSeverity WarningSeverity = "WARN"
transformSeverity InfoSeverity = "INFO"

-- | Transforms the path to a 'Text' representation (parts are seperated with a dot)
transformPath :: [Text] -> Text
transformPath = mconcat . intersperse "."

-- | This function can be used to tell the 'Generator' monad where in the OpenAPI specification the generator currently is
nested :: Text -> Generator a -> Generator a
nested pathItem = local $ \g -> g {currentPath = currentPath g <> [pathItem]}

createReferenceLookupM :: (Text -> ReferenceMap -> Maybe a) -> Text -> Generator (Maybe a)
createReferenceLookupM fn key = asks $ fn key . references

-- | Resolve a 'SchemaObject' reference from within the 'Generator' monad
getSchemaReferenceM :: Text -> Generator (Maybe SchemaObject)
getSchemaReferenceM = createReferenceLookupM getSchemaReference

-- | Resolve a 'ResponseObject' reference from within the 'Generator' monad
getResponseReferenceM :: Text -> Generator (Maybe ResponseObject)
getResponseReferenceM = createReferenceLookupM getResponseReference

-- | Resolve a 'ParameterObject' reference from within the 'Generator' monad
getParameterReferenceM :: Text -> Generator (Maybe ParameterObject)
getParameterReferenceM = createReferenceLookupM getParameterReference

-- | Resolve a 'ExampleObject' reference from within the 'Generator' monad
getExampleReferenceM :: Text -> Generator (Maybe ExampleObject)
getExampleReferenceM = createReferenceLookupM getExampleReference

-- | Resolve a 'RequestBodyObject' reference from within the 'Generator' monad
getRequestBodyReferenceM :: Text -> Generator (Maybe RequestBodyObject)
getRequestBodyReferenceM = createReferenceLookupM getRequestBodyReference

-- | Resolve a 'HeaderObject' reference from within the 'Generator' monad
getHeaderReferenceM :: Text -> Generator (Maybe HeaderObject)
getHeaderReferenceM = createReferenceLookupM getHeaderReference

-- | Resolve a 'SecuritySchemeObject' reference from within the 'Generator' monad
getSecuritySchemeReferenceM :: Text -> Generator (Maybe SecuritySchemeObject)
getSecuritySchemeReferenceM = createReferenceLookupM getSecuritySchemeReference

-- | Get all flags passed to the program
getFlags :: Generator Flags
getFlags = asks flags

-- | Get a specific flag selected by @f@
getFlag :: (Flags -> a) -> Generator a
getFlag f = asks $ f . flags
