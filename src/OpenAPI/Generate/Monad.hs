{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

-- | This module contains the 'Generator' monad and functions which deal with this monad.
-- In addition this module contains the means for logging and resolving references since they are
-- closely linked to the 'Generator' monad.
module OpenAPI.Generate.Monad where

import qualified Control.Monad.Reader as MR
import qualified Control.Monad.Writer as MW
import Data.List
import Data.Text (Text)
import qualified OpenAPI.Generate.Flags as OAF
import qualified OpenAPI.Generate.Reference as Ref
import qualified OpenAPI.Generate.Types as OAT
import qualified OpenAPI.Generate.Types.Schema as OAS

-- | The reader environment of the 'Generator' monad
--
-- The 'currentPath' is updated using the 'nested' function to track the current position within the specification.
-- This is used to produce tracable log messages.
-- The 'references' map is a lookup table for references within the OpenAPI specification.
data GeneratorEnvironment
  = GeneratorEnvironment
      { currentPath :: [Text],
        references :: Ref.ReferenceMap,
        flags :: OAF.Flags
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

-- | The 'Generator' monad is used to pass a 'MR.Reader' environment to functions in need of resolving references
-- and collects log messages.
newtype Generator a = Generator {unGenerator :: MW.WriterT GeneratorLogs (MR.Reader GeneratorEnvironment) a}
  deriving (Functor, Applicative, Monad, MR.MonadReader GeneratorEnvironment, MW.MonadWriter GeneratorLogs)

-- | Runs the generator monad within a provided environment.
runGenerator :: GeneratorEnvironment -> Generator a -> (a, GeneratorLogs)
runGenerator e (Generator g) = MR.runReader (MW.runWriterT g) e

-- | Create an environment based on a 'Ref.ReferenceMap' and 'OAF.Flags'
createEnvironment :: OAF.Flags -> Ref.ReferenceMap -> GeneratorEnvironment
createEnvironment flags references =
  GeneratorEnvironment
    { currentPath = [],
      references = references,
      flags = flags
    }

-- | Writes a log message to a 'Generator' monad
logMessage :: GeneratorLogSeverity -> Text -> Generator ()
logMessage severity message = do
  path' <- MR.asks currentPath
  MW.tell [GeneratorLogEntry {path = path', severity = severity, message = message}]

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
nested pathItem = MR.local $ \g -> g {currentPath = currentPath g <> [pathItem]}

-- | Helper function to create a lookup function for a specific type
createReferenceLookupM :: (Text -> Ref.ReferenceMap -> Maybe a) -> Text -> Generator (Maybe a)
createReferenceLookupM fn key = MR.asks $ fn key . references

-- | Resolve a 'OAS.SchemaObject' reference from within the 'Generator' monad
getSchemaReferenceM :: Text -> Generator (Maybe OAS.SchemaObject)
getSchemaReferenceM = createReferenceLookupM Ref.getSchemaReference

-- | Resolve a 'OAT.ResponseObject' reference from within the 'Generator' monad
getResponseReferenceM :: Text -> Generator (Maybe OAT.ResponseObject)
getResponseReferenceM = createReferenceLookupM Ref.getResponseReference

-- | Resolve a 'OAT.ParameterObject' reference from within the 'Generator' monad
getParameterReferenceM :: Text -> Generator (Maybe OAT.ParameterObject)
getParameterReferenceM = createReferenceLookupM Ref.getParameterReference

-- | Resolve a 'OAT.ExampleObject' reference from within the 'Generator' monad
getExampleReferenceM :: Text -> Generator (Maybe OAT.ExampleObject)
getExampleReferenceM = createReferenceLookupM Ref.getExampleReference

-- | Resolve a 'OAT.RequestBodyObject' reference from within the 'Generator' monad
getRequestBodyReferenceM :: Text -> Generator (Maybe OAT.RequestBodyObject)
getRequestBodyReferenceM = createReferenceLookupM Ref.getRequestBodyReference

-- | Resolve a 'OAT.HeaderObject' reference from within the 'Generator' monad
getHeaderReferenceM :: Text -> Generator (Maybe OAT.HeaderObject)
getHeaderReferenceM = createReferenceLookupM Ref.getHeaderReference

-- | Resolve a 'OAT.SecuritySchemeObject' reference from within the 'Generator' monad
getSecuritySchemeReferenceM :: Text -> Generator (Maybe OAT.SecuritySchemeObject)
getSecuritySchemeReferenceM = createReferenceLookupM Ref.getSecuritySchemeReference

-- | Get all flags passed to the program
getFlags :: Generator OAF.Flags
getFlags = MR.asks flags

-- | Get a specific flag selected by @f@
getFlag :: (OAF.Flags -> a) -> Generator a
getFlag f = MR.asks $ f . flags
