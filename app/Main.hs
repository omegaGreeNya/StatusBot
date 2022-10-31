{-# LANGUAGE RecordWildCards #-}
module Main (main) where

import Control.Concurrent (ThreadId, killThread, forkOS)
import Data.Maybe (catMaybes)

import Constants (configPath)
import Initialization
   ( AppConfig, initApp, withTelegramAPIHandle, withLoggerConfig
   , telegramFrontEnabled, getFeedbackAddress)

import qualified App
import qualified Front.ConsoleHTTP as ConsoleHTTP 
import qualified Front.TelegramHTTP as TelegramHTTP
import qualified Logger
import qualified Status.Implementation as Status


main :: IO ()
main = do
   appCfg <- initApp
   withLoggerConfig appCfg $ \loggerCfg -> do
      let hLogger = Logger.createHandle loggerCfg
      threads <- runExternalFronts appCfg hLogger
      warnOnNoFronts threads
      runConsoleAdmin appCfg hLogger
      mapM_ killThread threads

-- | Runs all external fonts that allowed by config 
-- in separate IO thread. Returns their thread ids.
runExternalFronts :: AppConfig -> Logger.Handle IO -> IO [ThreadId]
runExternalFronts appCfg hLogger = do
   tgThread <-
      if (telegramFrontEnabled appCfg)
      then do
         thread <- forkOS (runTelegramFront appCfg hLogger)
         putStrLn "Telegram bot enabled"
         return $ Just thread
      else return Nothing
   return . catMaybes $ tgThread : []

-- | Runs telegram front in forever loop.
runTelegramFront :: AppConfig -> Logger.Handle IO -> IO ()
runTelegramFront appCfg hLogger = do
   withTelegramAPIHandle appCfg hLogger $ \hAPITg -> do
      let hFront = TelegramHTTP.createHandle hAPITg hLogger
          hStatus = Status.createHandle hLogger (getFeedbackAddress appCfg)
      App.runAppSimpleForever App.Handle{..}

-- | Console is a bit special, sice we want to control over app
-- and it's functional simultaneously.
-- So this function listens to console input
-- and runs console front on getStatus command.
runConsoleAdmin :: AppConfig -> Logger.Handle IO -> IO ()
runConsoleAdmin appCfg hLogger = do
   let hFront  = ConsoleHTTP.createHandle hLogger
       hStatus = Status.createHandle hLogger (getFeedbackAddress appCfg)
   putStrLn "Console bot enabled"
   App.runAppAdmin App.Handle{..}

warnOnNoFronts :: [a] -> IO ()
warnOnNoFronts [] = 
   putStrLn 
      $ "Warning: No fronts enabled, you can configure them throught " <> configPath <> "."
warnOnNoFronts _ = return ()