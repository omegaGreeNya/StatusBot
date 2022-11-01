{-# LANGUAGE RecordWildCards #-}
-- | Module provides logging functions
-- and hides it's internals.
module Logger
    ( Handle
    , LogLevel(..)
    , logDebug
    , logInfo
    , logWarning
    , logError
    ) where

import Data.Text (Text)

import Logger.Handle(Handle(..), LogLevel(..))

-- | Logging aliases.
logDebug, logInfo, logWarning, logError :: Handle m -> Text -> m ()
logDebug   h = hLogMessage h DEBUG
logInfo    h = hLogMessage h INFO
logWarning h = hLogMessage h WARN
logError   h = hLogMessage h ERROR