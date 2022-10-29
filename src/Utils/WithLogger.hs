-- | Different funtions to make life easier.
module Utils.WithLogger
   ( withSleep
   ) where

import Control.Concurrent (threadDelay)
import Control.Monad (when)
import Control.Monad.IO.Class (MonadIO, liftIO)
import Data.Text (Text)

import Logger (logDebug)
import Utils (getCurrentTimeHiRes, (.<))

import qualified Logger

-- Temporary
withSleep _ _ time act = do
   result <- act
   liftIO $ threadDelay time
   return result

{-
-- | Does action and freezes thred for rested time.
withSleep :: MonadIO m
          => Text
          -- ^ Action name, used for logs.
          -> Logger.Handle m
          -- ^ Loger handle.
          -> Int
          -- ^ Max time in miliseconds for this action.
          -> m a
          -- ^ Action to perform.
          -> m a
          -- Action result.
withSleep actionName hLogger maxSleepTime action = do
   startTime <- getCurrentTimeHiRes
   logDebug hLogger $ actionName <> " started at " .< startTime <> "."
   
   result <- action
   
   finishTime <- getCurrentTimeHiRes
   logDebug hLogger $ actionName <> " finished at " .< finishTime <> "."
   
   let timeToSleep = maxSleepTime - (finishTime - startTime)
   when (timeToSleep > 0)
      $ liftIO $ threadDelay timeToSleep
   
   return result
-}