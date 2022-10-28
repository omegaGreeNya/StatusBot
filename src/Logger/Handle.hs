-- | Logger interface.
module Logger.Handle
    ( Handle(..)
    , LogLevel(..)
    , logDebug
    , logInfo
    , logWarning
    , logError
    ) where

import Data.Text (Text)

data Handle m = Handle
   { hLogMessage :: LogLevel -> Text -> m ()
   -- ^ Actual implementation abstracted
   -- behind this function.
   }

data LogLevel
   = DEBUG
   | INFO
   | WARN
   | ERROR
   deriving (Show, Eq, Ord)

-- | Logging aliases.
logDebug, logInfo, logWarning, logError :: Handle m -> Text -> m ()
logDebug   h = hLogMessage h DEBUG
logInfo    h = hLogMessage h INFO
logWarning h = hLogMessage h WARN
logError   h = hLogMessage h ERROR