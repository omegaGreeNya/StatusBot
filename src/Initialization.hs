{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TemplateHaskell #-}
-- | Different funtions to make life easier.
module Initialization
   ( initApp
   , withLoggerConfig
   ) where

import Control.Exception (tryJust)
import Control.Monad (guard)
import Control.Monad.IO.Class (MonadIO, liftIO)
import Data.Aeson (encode, eitherDecode)
import Data.Aeson.TH (deriveJSON)
import Data.Text (Text)
import System.IO (IOMode(..), openFile)
import System.IO.Error (isDoesNotExistError)

import qualified Data.ByteString.Lazy.Char8 as BC
import qualified Data.Text.IO as T
import qualified System.IO as File (Handle)

import Constants (configPath)
import Utils (derivingDrop)

import qualified Logger


data LoggerConfig = LoggerConfig
   { cfg_consoleLogging :: Bool
   , cfg_fileLogging    :: Bool
   , cfg_logFile        :: FilePath
   , cfg_logLevel       :: Text
   , cfg_formatter      :: Text
   } deriving (Show)

deriveJSON (derivingDrop 4) ''LoggerConfig

data AppConfig = AppConfig
   { cfg_logger :: LoggerConfig
   } deriving (Show)

deriveJSON (derivingDrop 4) ''AppConfig

defToConsole :: Bool
defToConsole = True

defToFile :: Bool
defToFile = True

defLogFile :: FilePath
defLogFile = "Log.txt"

defLogLevel :: Text
defLogLevel = "INFO"

defFormatter :: Text
defFormatter = "DATE"

defaultLoggerConfig :: LoggerConfig
defaultLoggerConfig = LoggerConfig   
   { cfg_consoleLogging = defToConsole
   , cfg_fileLogging    = defToFile
   , cfg_logFile        = defLogFile
   , cfg_logLevel       = defLogLevel
   , cfg_formatter      = defFormatter
   }

defaultAppConfig :: AppConfig
defaultAppConfig = AppConfig
   { cfg_logger = defaultLoggerConfig
   }

-- << Interface

-- | Tries to init app config from config file,
-- is case of missing/corrapted config returns default.
-- May trow exception in unexpated cases.
initApp :: IO AppConfig
initApp = do
   eCfgRaw <- tryJust (guard . isDoesNotExistError)
                 $ BC.readFile configPath
   case eCfgRaw of
      Left _ -> handleMissingConfigFile
      Right cfgRaw -> do
         case eitherDecode cfgRaw of
            Left err -> handleDecodingErr err
            Right appCfg -> return appCfg

-- | Provides logger configuration.
-- Closes log file handle after usage if needed.
withLoggerConfig
   :: MonadIO m => AppConfig -> (Logger.Config m -> m a) -> m a
withLoggerConfig appCfg f = do
   logCfg <- createLoggerConfig appCfg
   result <- f logCfg
   Logger.shutdownConfig logCfg
   return result

-- >>

-- << Implementation

-- | Interprets AppConfig into logger config.
-- Corrupted values would be substitute to dafaults.
-- Not exposed, since function opens handle.
createLoggerConfig :: MonadIO m => AppConfig -> m (Logger.Config m)
createLoggerConfig AppConfig{..} = do
   let LoggerConfig{..} = cfg_logger
       consoleLoggingAllowed = cfg_consoleLogging
   fileLogging <- if cfg_fileLogging
      then fmap Just $ createLogFileHandle cfg_logFile
      else return Nothing
   let cfgConnectionHandle = Logger.OutputHandle{..}
   
   cfgMinLogLeveL <- pickLogLevel cfg_logLevel
   cfgFormatter <- pickFormatter cfg_formatter
   
   return Logger.Config{..}

-- | Prints missing file error to console.
-- Writes and returns default config.
handleMissingConfigFile :: IO AppConfig
handleMissingConfigFile = do
   putStrLn $ "Missing \"" <> configPath <> "\". Default config would be created."
   let cfgRaw = encode defaultAppConfig
   BC.writeFile configPath cfgRaw
   return defaultAppConfig

-- | Prints decoding error message to console
-- and returns default config.
handleDecodingErr :: String -> IO AppConfig
handleDecodingErr err = do
   putStrLn $ "Can't decode config: " <> err
   putStrLn $ "Default config would be used instead."
   return defaultAppConfig

-- | Creates log file handle.
-- May trow unexpected error.
createLogFileHandle :: MonadIO m => FilePath -> m File.Handle
createLogFileHandle path = liftIO $ openFile path AppendMode

-- | Parses config log level field into logger min log level.
pickLogLevel :: MonadIO m => Text -> m Logger.LogLevel
pickLogLevel txt 
   | txt == "DEBUG" = return Logger.DEBUG
   | txt == "INFO" = return Logger.INFO
   | txt == "WARN" = return Logger.WARN
   | txt == "ERROR" = return Logger.ERROR
   | otherwise = do
      liftIO $ T.putStrLn 
         $ "Can't parse minimal logging level: \""
         <> txt
         <> "\"."
         <> defLogLevel
         <> " would be used intead."
      pickLogLevel defLogLevel

-- | Parses config formatter field into logger formatter.
pickFormatter :: MonadIO m => Text -> m (Logger.Formatter m)
pickFormatter txt
   | txt == "DATE" = return Logger.dateFormating
   | txt == "SIMPLE" = return Logger.simpleFormatting
   | txt == "NO_FORMATTING" = return Logger.noFormatting
   | otherwise = do
      liftIO $ T.putStrLn 
         $ "Can't parse formatter type: \""
         <> txt
         <> "\"."
         <> defFormatter
         <> " would be used intead."
      pickFormatter defFormatter

-- >>