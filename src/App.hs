{-# LANGUAGE RecordWildCards #-}
-- | App
module App
   ( runAppSimpleForever
   ) where

import Control.Concurrent (threadDelay)
import Control.Monad (forever)
import Control.Monad.IO.Class (MonadIO, liftIO)

import qualified Front as Front


-- | Runs app in forever loop.
runAppSimpleForever
   :: (MonadIO m, Show user, Show adress)
   => Front.Handle user adress m
   -> m ()
runAppSimpleForever hFront =
   forever $ do
      Front.runFrontOnce hFront
      liftIO $ threadDelay 1000