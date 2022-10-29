module Main (main) where

import Initialization (initApp, withLoggerConfig)
import App (runAppSimpleForever)


import qualified Logger (createHandle)
import qualified ConsoleHTTP (createHandle)

main :: IO ()
main = do
   appCfg <- initApp
   withLoggerConfig appCfg $ \loggerCfg -> do
      let hLogger = Logger.createHandle loggerCfg
      let hConsole = ConsoleHTTP.createHandle hLogger
      runAppSimpleForever hConsole