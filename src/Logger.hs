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
    , mockedHandle
    , noFormatting
    , simpleFormatting
    , dateFormating
    , logDebug
    , logInfo
    , logWarning
    , logError
    ) where

import Control.Monad.IO.Class (MonadIO, liftIO)
import Data.Text (Text)
import Data.Time.Clock (getCurrentTime)
import System.IO (openFile)

import qualified Data.Text.IO as T
import qualified System.IO as File (Handle, IOMode(..))

import Logger.Handle
   (Handle(..), LogLevel(..), logDebug
   , logInfo, logWarning, logError)
import Utils ((.<))

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

-- | Creates Logger Handle from provided Config.
createHandle :: MonadIO m => Config m -> Handle m
createHandle cfg = Handle $ logger cfg

-- | Suitable for testing. Does Nothing.
mockedHandle :: Monad m => Handle m
mockedHandle = Handle (\_ _ -> return ())

-- | Does nothing with the messages.
noFormatting :: Monad m => LogLevel -> Text -> m Text
noFormatting _ = return

-- | Adds log level to messages.
simpleFormatting :: Monad m => LogLevel -> Text -> m Text
simpleFormatting lvl text =
   return $ "[" .< lvl <> "] " <> text

-- | Adds current UTC time and log level to messages.
dateFormating :: MonadIO m => LogLevel -> Text -> m Text
dateFormating lvl text = do
   currentUTCTime <- liftIO getCurrentTime
   return $ "[" .< currentUTCTime <> " " .< lvl <> "] " <> text
   

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
   | cfgMinLogLeveL <= textLogLvl = formattedText >>= loggerRaw cfgConnectionHandle
   | otherwise                    = return ()
   where
      formattedText = cfgFormatter textLogLvl textToLog

-- | Puts log message to destination defined by 'OutHandle'.
loggerRaw :: MonadIO m => OutHandle -> Text -> m ()
loggerRaw Console = liftIO . T.putStrLn
loggerRaw (File hFile) = liftIO . T.hPutStrLn hFile
-- >>