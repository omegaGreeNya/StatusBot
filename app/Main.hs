{-# LANGUAGE RecordWildCards #-}
module Main (main) where

import Control.Concurrent (ThreadId, killThread, forkOS)
import Data.Text (Text)
import Data.Maybe (catMaybes)

import qualified Data.Text as T
import qualified Data.Text.IO as T

import Front.ConsoleHTTP (ConsoleHTTP)
import Initialization
   ( AppConfig, initApp, withTelegramAPIHandle, withLoggerConfig
   , consoleFrontEnabled, telegramFrontEnabled)

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
      if (consoleFrontEnabled appCfg)
         then runConsoleFront hLogger
         else listenCommands
      mapM_ killThread threads

-- | Runs all external fonts that allowed by config 
-- in separate IO thread. Returns their thread ids.
runExternalFronts :: AppConfig -> Logger.Handle IO -> IO [ThreadId]
runExternalFronts appCfg hLogger = do
   tgThread <-
      if (telegramFrontEnabled appCfg)
      then do
         thread <- forkOS (runTelegramFront appCfg hLogger)
         T.putStrLn "Telegram bot enabled"
         return $ Just thread
      else return Nothing
   return . catMaybes $ tgThread : []

-- | Runs telegram front in forever loop.
runTelegramFront :: AppConfig -> Logger.Handle IO -> IO ()
runTelegramFront appCfg hLogger = do
   withTelegramAPIHandle appCfg hLogger $ \hAPITg -> do
      let hFront = TelegramHTTP.createHandle hAPITg hLogger
          hStatus = Status.createHandle hLogger
      App.runAppSimpleForever App.Handle{..}

-- | Console is a bit special, sice we want to control over app
-- and it's functional simultaneously.
-- So this function listens to console input
-- and runs console front on getStatus command.
runConsoleFront :: Logger.Handle IO -> IO ()
runConsoleFront hLogger = do
   let front  = ConsoleHTTP.createHandleWithProvidedInput hLogger
       hStatus = Status.createHandle hLogger
   T.putStrLn "Console bot enabled"
   printHelpConsole
   listenConsoleCommands -- Dirty hack.
      $ \input -> let hFront = front input in App.Handle{..}


-- | Controlls app throught console input,
-- and acts as console tool on getStatus command.
listenConsoleCommands :: (Text -> App.Handle ConsoleHTTP IO) -> IO ()
listenConsoleCommands hApp = do
   input <- T.getLine
   case T.words input of
      ("stop":_)
         -> return ()
      ("help":_)
         -> printHelpConsole >> listenConsoleCommands hApp
      ("getStatus":rest)
         -> App.runAppSimple (hApp $ T.unwords rest)
         >> listenConsoleCommands hApp
      _
         -> listenConsoleCommands hApp

-- | Controlls app throught console input.
-- Useful if console disabled, or we want kind of silent mode.
listenCommands :: IO ()
listenCommands = printHelpGeneric >> listenCommands'

listenCommands' :: IO ()
listenCommands' =  do
   input <- T.getLine
   case T.words input of
      ("stop":_)
         -> return ()
      ("help":_)
         -> printHelpGeneric >> listenCommands'
      _
         -> listenCommands'

printHelpConsole :: IO ()
printHelpConsole = do
   printHelpGeneric
   T.putStr $ T.unlines 
      ["getStatus <IP> - asks for server status."]

printHelpGeneric :: IO ()
printHelpGeneric =
   T.putStr $ T.unlines 
      [ "help - prints this message"
      , "stop - to stop app. Note, it may take time to close http calls."
      ]