{-# LANGUAGE RecordWildCards #-}
module Main (main) where

import Control.Concurrent (ThreadId, killThread, forkOS)
import Control.Monad (when)
import Data.Maybe (catMaybes)

import Initialization
   ( AppConfig, initApp, withTelegramAPIHandle, withLoggerConfig
   , consoleFrontEnabled, telegramFrontEnabled)

import qualified App
import qualified Front.ConsoleHTTP as ConsoleHTTP (createHandle) 
import qualified Front.TelegramHTTP as TelegramHTTP (createHandle)
import qualified Logger
import qualified Status.Implementation as Status


main :: IO ()
main = do
   appCfg <- initApp
   print appCfg
   withLoggerConfig appCfg $ \loggerCfg -> do
      print $ Logger.cfgConnectionHandle loggerCfg
      threads <- runAllFronts appCfg loggerCfg
      listenCommands
      mapM_ killThread threads

runAllFronts :: AppConfig -> Logger.Config IO -> IO [ThreadId]
runAllFronts appCfg loggerCfg = do
   tgThread <- if (telegramFrontEnabled appCfg)
               then fmap Just $ forkOS (runTelegramFront appCfg loggerCfg)
               else return Nothing
   when (consoleFrontEnabled appCfg)
      $ runConsoleFront loggerCfg
   return . catMaybes $ tgThread : []

runConsoleFront :: Logger.Config IO -> IO ()
runConsoleFront loggerCfg = do
   let hLogger = Logger.createHandle loggerCfg
       hFront = ConsoleHTTP.createHandle hLogger
       hStatus = Status.createHandle hLogger
   App.runAppSimpleForever App.Handle{..}

runTelegramFront :: AppConfig -> Logger.Config IO -> IO ()
runTelegramFront appCfg loggerCfg = do
   let hLogger = Logger.createHandle loggerCfg
   withTelegramAPIHandle appCfg hLogger $ \hAPITg -> do
      let hFront = TelegramHTTP.createHandle hAPITg hLogger
          hStatus = Status.createHandle hLogger
      App.runAppSimpleForever App.Handle{..}

listenCommands :: IO ()
listenCommands = do
   input <- getLine
   case input of
      "stop" -> return ()
      _      -> listenCommands