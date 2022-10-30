module Main (main) where

import Control.Concurrent (ThreadId, forkOS)
import Data.Maybe (catMaybes)

import Initialization
   ( AppConfig, initApp, withTelegramAPIHandle, withLoggerConfig
   , consoleFrontEnabled, telegramFrontEnabled)
import App (runAppSimpleForever)

import qualified Logger (createHandle)
import qualified Front.ConsoleHTTP as ConsoleHTTP (createHandle) 
import qualified Front.TelegramHTTP as TelegramHTTP (createHandle)


main :: IO ()
main = do
   appCfg <- initApp
   _ <- runAllFronts appCfg
   return ()

runAllFronts :: AppConfig -> IO [ThreadId]
runAllFronts appCfg = do
   tgThread <- if (telegramFrontEnabled appCfg)
               then fmap Just $ forkOS (runTelegramFront appCfg)
               else return Nothing
   cliThread <- if (consoleFrontEnabled appCfg)
                then fmap Just $ forkOS (runConsoleFront appCfg)
                else return Nothing
   return . catMaybes $ tgThread : cliThread : []

runConsoleFront :: AppConfig -> IO ()
runConsoleFront appCfg = do
   withLoggerConfig appCfg $ \loggerCfg -> do
      let hLogger = Logger.createHandle loggerCfg
          hConsole = ConsoleHTTP.createHandle hLogger
      runAppSimpleForever hConsole

runTelegramFront :: AppConfig -> IO ()
runTelegramFront appCfg = do
   withTelegramAPIHandle appCfg $ \hAPITg -> 
      withLoggerConfig appCfg $ \loggerCfg -> do
         let hLogger = Logger.createHandle loggerCfg
             hTelegram = TelegramHTTP.createHandle hAPITg hLogger
         runAppSimpleForever hTelegram
   