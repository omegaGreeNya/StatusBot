{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TemplateHaskell #-}
-- | Module defines app configuration, 
-- parsing config file and app initialization.
module Initialization
   ( AppConfig
   , initApp
   
   , consoleFrontEnabled
   , telegramFrontEnabled
   
   , withLoggerConfig
   , withTelegramAPIHandle
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
import qualified API.Telegram


data LoggerConfig = LoggerConfig
   { cfg_consoleLogging :: Bool
   , cfg_fileLogging    :: Bool
   , cfg_logFile        :: FilePath
   , cfg_logLevel       :: Text
   , cfg_formatter      :: Text
   } deriving (Show)

deriveJSON (derivingDrop 4) ''LoggerConfig

data TelegramConfig = TelegramConfig
   { cfg_token   :: Text
   , cfg_timeout :: Int
   } deriving (Show)

deriveJSON (derivingDrop 4) ''TelegramConfig

data FrontConfig = FrontConfig
   { cfg_consoleUsage  :: Bool
   , cfg_telegramUsage :: Bool
   , cfg_telegram      :: TelegramConfig
   } deriving (Show)

deriveJSON (derivingDrop 4) ''FrontConfig

data AppConfig = AppConfig
   { cfg_logger :: LoggerConfig
   , cfg_front  :: FrontConfig
   } deriving (Show)

deriveJSON (derivingDrop 4) ''AppConfig

-- << Logger default
defLogToConsole :: Bool
defLogToConsole = False

defLogToFile :: Bool
defLogToFile = True

defLogFile :: FilePath
defLogFile = "Log.txt"

defLogLevel :: Text
defLogLevel = "INFO"

defLogFormatter :: Text
defLogFormatter = "DATE"

defaultLoggerConfig :: LoggerConfig
defaultLoggerConfig = LoggerConfig   
   { cfg_consoleLogging = defLogToConsole
   , cfg_fileLogging    = defLogToFile
   , cfg_logFile        = defLogFile
   , cfg_logLevel       = defLogLevel
   , cfg_formatter      = defLogFormatter
   }
-- >> 
-- << Front default
defFrontToConsole :: Bool
defFrontToConsole = True

defFrontToTelegram :: Bool
defFrontToTelegram = False

defTelegramToken :: Text
defTelegramToken = ""

defTelegramTimeout :: Int
defTelegramTimeout = 60

defaultTelegramConfig :: TelegramConfig
defaultTelegramConfig = TelegramConfig
   { cfg_token = defTelegramToken
   , cfg_timeout = defTelegramTimeout
   }

defaultFrontConfig :: FrontConfig
defaultFrontConfig = FrontConfig
   { cfg_consoleUsage  = defFrontToConsole
   , cfg_telegramUsage = defFrontToTelegram
   , cfg_telegram      = defaultTelegramConfig
   }
-- >>

defaultAppConfig :: AppConfig
defaultAppConfig = AppConfig
   { cfg_logger = defaultLoggerConfig
   , cfg_front  = defaultFrontConfig
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

consoleFrontEnabled :: AppConfig -> Bool
consoleFrontEnabled AppConfig{..} =
   cfg_consoleUsage cfg_front

telegramFrontEnabled :: AppConfig -> Bool
telegramFrontEnabled AppConfig{..} =
   cfg_telegramUsage cfg_front

withTelegramAPIHandle
   :: MonadIO m
   => AppConfig
   -> Logger.Handle m
   -> (API.Telegram.Handle m -> m a)
   -> m a
withTelegramAPIHandle AppConfig{..} hLogger f = do
   let TelegramConfig{..} = cfg_telegram cfg_front
   hAPITg <- API.Telegram.createHandle hLogger cfg_token cfg_timeout
   f hAPITg

-- >>

-- << Implementation

-- | Interprets AppConfig into logger config.
-- Corrupted values would be substitute to defaults.
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
         <> defLogFormatter
         <> " would be used intead."
      pickFormatter defLogFormatter

-- >>