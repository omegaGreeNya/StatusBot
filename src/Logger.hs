{-# LANGUAGE RecordWildCards #-}
-- | Module defines logger implementation
-- and it's initialization interface.

-- TO-DO
-- Fix FLAW
--    file to log to may not exist.
module Logger
    ( Config
    , Handle
    , createConsoleConfig
    , createFileConfig
    , createHandle
    , logDebug
    , logInfo
    , logWarning
    , logError
    ) where

import Control.Monad.IO.Class (MonadIO, liftIO)
import Data.Text (Text)
import System.IO (openFile)

import qualified Data.Text.IO as T
import qualified System.IO as File (Handle, IOMode(..))

import Logger.Handle
   (Handle(..), LogLevel(..), logDebug
   , logInfo, logWarning, logError)

-- | Output handle.
data OutHandle
   = Console
   | File File.Handle

data Config m = Config
   { cfgMinLogLeveL      :: LogLevel
   , cfgFormatter        :: LogLevel -> Text -> m Text
   , cfgConnectionHandle :: OutHandle
   }


-- << Interface

-- | Creates config to log into provided filepath.
-- If file doesn't exist, it would be created.
-- File would be opened with appended mode.
createFileConfig :: MonadIO m => LogLevel -> (LogLevel -> Text -> m Text) -> FilePath -> m (Config m)
createFileConfig cfgMinLogLeveL cfgFormatter filePath = do
   hFile <- liftIO $ openFile filePath File.AppendMode -- FLAW
   let cfgConnectionHandle = File hFile
   return Config{..}

-- | Creates config to log into console
createConsoleConfig :: MonadIO m => LogLevel -> (LogLevel -> Text -> m Text) -> m (Config m)
createConsoleConfig cfgMinLogLeveL cfgFormatter = do
   let cfgConnectionHandle = Console
   return Config{..}


createHandle :: MonadIO m => Config m -> Handle m
createHandle cfg = Handle $ logger cfg

-- >>

-- << Implementation

-- | Logs with provided function.
-- If log level less than defined, message would
-- be ignored.
logger :: MonadIO m
       => Config m
       -> LogLevel
       -> Text
       -> m ()
logger Config{..} textLogLvl textToLog
   | cfgMinLogLeveL <= textLogLvl = formattedText >>= hLog cfgConnectionHandle
   | otherwise                    = return ()
   where
      formattedText = cfgFormatter textLogLvl textToLog

-- | Puts log message to destination defined by 'OutHandle'.
hLog :: MonadIO m => OutHandle -> Text -> m ()
hLog Console = liftIO . T.putStrLn
hLog (File hFile) = liftIO . T.hPutStrLn hFile
-- >>