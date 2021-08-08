{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

module OpenAPI.Generate.Log where

import Control.Applicative
import Data.List (intersperse)
import Data.Text (Text)
import qualified Data.Text as T
import YamlParse.Applicative

-- | Data type representing the log severities
data LogSeverity
  = TraceSeverity
  | InfoSeverity
  | WarningSeverity
  | ErrorSeverity
  deriving (Eq, Ord)

instance Show LogSeverity where
  show ErrorSeverity = "ERROR"
  show WarningSeverity = "WARN"
  show InfoSeverity = "INFO"
  show TraceSeverity = "TRACE"

instance Read LogSeverity where
  readsPrec _ ('E' : 'R' : 'R' : 'O' : 'R' : rest) = [(ErrorSeverity, rest)]
  readsPrec _ ('W' : 'A' : 'R' : 'N' : rest) = [(WarningSeverity, rest)]
  readsPrec _ ('I' : 'N' : 'F' : 'O' : rest) = [(InfoSeverity, rest)]
  readsPrec _ ('T' : 'R' : 'A' : 'C' : 'E' : rest) = [(TraceSeverity, rest)]
  readsPrec _ _ = []

instance YamlSchema LogSeverity where
  yamlSchema =
    TraceSeverity <$ literalString (toText TraceSeverity)
      <|> InfoSeverity <$ literalString (toText InfoSeverity)
      <|> WarningSeverity <$ literalString (toText WarningSeverity)
      <|> ErrorSeverity <$ literalString (toText ErrorSeverity)
    where
      toText = T.pack . show

-- | A log entry containing the location within the OpenAPI specification where the message was produced, a severity and the actual message.
data LogEntry = LogEntry
  { logEntryPath :: [Text],
    logEntrySeverity :: LogSeverity,
    logEntryMessage :: Text
  }
  deriving (Show, Eq)

-- | The type contained in the writer of the 'OpenAPI.Generate.Monad.Generator' used to collect log entries
type LogEntries = [LogEntry]

-- | Filters and transforms log entries for printing
filterAndTransformLogs :: LogSeverity -> LogEntries -> [Text]
filterAndTransformLogs minimumLogLevel = transformLogs . filterLogs minimumLogLevel

-- | Filters log entries which have a lower log level than provided
filterLogs :: LogSeverity -> LogEntries -> LogEntries
filterLogs minimumLogLevel = filter $ (>= minimumLogLevel) . logEntrySeverity

-- | Transforms 'LogEntries' to a list of 'Text' values for easier printing.
transformLogs :: LogEntries -> [Text]
transformLogs =
  fmap
    ( \LogEntry {..} ->
        T.justifyLeft 5 ' ' (T.pack $ show logEntrySeverity) <> " (" <> transformPath logEntryPath <> "): " <> logEntryMessage
    )

-- | Transforms the path to a 'Text' representation (parts are seperated with a dot)
transformPath :: [Text] -> Text
transformPath = mconcat . intersperse "."
