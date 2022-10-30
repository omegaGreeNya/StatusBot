module Main (main) where

import Control.Concurrent (ThreadId, killThread, forkOS)
import Control.Monad (when)
import Data.Maybe (catMaybes)

import Initialization
   ( AppConfig, initApp, withTelegramAPIHandle, withLoggerConfig
   , consoleFrontEnabled, telegramFrontEnabled)
import App (runAppSimpleForever)

import qualified Logger --(Config, createHandle)
import qualified Front.ConsoleHTTP as ConsoleHTTP (createHandle) 
import qualified Front.TelegramHTTP as TelegramHTTP (createHandle)


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
       hConsole = ConsoleHTTP.createHandle hLogger
   runAppSimpleForever hConsole

runTelegramFront :: AppConfig -> Logger.Config IO -> IO ()
runTelegramFront appCfg loggerCfg =
   withTelegramAPIHandle appCfg $ \hAPITg -> do
      let hLogger = Logger.createHandle loggerCfg
          hTelegram = TelegramHTTP.createHandle hAPITg hLogger
      runAppSimpleForever hTelegram

listenCommands :: IO ()
listenCommands = do
   input <- getLine
   case input of
      "stop" -> return ()
      _      -> listenCommands