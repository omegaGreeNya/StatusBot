{-# LANGUAGE RecordWildCards #-}
-- | App
module App
   ( runAppSimpleForever
   ) where

import Control.Monad (forever)
import Control.Monad.IO.Class (MonadIO)

import Utils.WithLogger (withSleep)

import qualified Front.Handle as Front


-- | Runs app in forever loop.
runAppSimpleForever :: (MonadIO m, Show user, Show adress) => Front.Handle user adress m -> m ()
runAppSimpleForever hFront@Front.Handle{..} =
   forever . (withSleep "TEST" hLogger 1000) $
      Front.runFrontOnce hFront