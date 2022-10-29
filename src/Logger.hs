{-# LANGUAGE RecordWildCards #-}
-- | Module defines logger implementation
-- and it's initialization interface.
module Logger
    ( Config(..)
    , Formatter
    , Handle
    , LogLevel(..)
    , OutputHandle(..)
    , createHandle
    , mockedHandle
    , shutdownConfig
    , noFormatting
    , simpleFormatting
    , dateFormating
    , logDebug
    , logInfo
    , logWarning
    , logError
    ) where

import Control.Monad (when)
import Control.Monad.IO.Class (MonadIO, liftIO)
import Data.Text (Text)
import Data.Time.Clock (getCurrentTime)

import qualified Data.Text.IO as T
import qualified System.IO as File (hClose, Handle)

import Logger.Handle
   (Handle(..), LogLevel(..), logDebug
   , logInfo, logWarning, logError)
import Utils ((.<))

-- | Output handle.
data OutputHandle = OutputHandle
   { consoleLoggingAllowed :: Bool
   , fileLogging           :: Maybe File.Handle
   }

type Formatter m = LogLevel -> Text -> m Text

data Config m = Config
   { cfgMinLogLeveL      :: LogLevel
   , cfgFormatter        :: Formatter m
   , cfgConnectionHandle :: OutputHandle
   }


-- << Interface

-- | Creates Logger Handle from provided Config.
createHandle :: MonadIO m => Config m -> Handle m
createHandle cfg = Handle $ logger cfg

-- | Suitable for testing. Does Nothing.
mockedHandle :: Monad m => Handle m
mockedHandle = Handle (\_ _ -> return ())

-- | Does nothing with the messages.
noFormatting :: Monad m => Formatter m
noFormatting _ = return

-- | Adds log level to messages.
simpleFormatting :: Monad m => Formatter m
simpleFormatting lvl text =
   return $ "[" .< lvl <> "] " <> text

-- | Adds current UTC time and log level to messages.
dateFormating :: MonadIO m => Formatter m
dateFormating lvl text = do
   currentUTCTime <- liftIO getCurrentTime
   return $ "[" .< currentUTCTime <> " " .< lvl <> "] " <> text

-- | Closes log file handle if config contains one.
shutdownConfig :: MonadIO m => Config m -> m ()
shutdownConfig Config{..} =
   case fileLogging cfgConnectionHandle of
      Nothing -> return ()
      Just hFile -> liftIO $ File.hClose hFile

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

-- | Puts log message to destination defined by 'OutputHandle'.
loggerRaw :: MonadIO m => OutputHandle -> Text -> m ()
loggerRaw OutputHandle{..} text = do
   when consoleLoggingAllowed 
      $ liftIO $ T.putStrLn text
   case fileLogging of
      Nothing -> return ()
      Just hFile -> liftIO $ T.hPutStrLn hFile text
-- >>