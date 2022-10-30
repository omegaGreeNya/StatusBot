{-# LANGUAGE RecordWildCards #-}
-- | Moodule defines implementation of status handle functions.
module Status
    ( Handle
    , ServerStatus(..)
    , getStatus
    ) where

import Logger (logInfo)
import Status.Handle (Handle(..), ServerStatus(..))
import Utils ((.<))

-- | Returns status and logs action.
getStatus
   :: (Monad m, Show adress)
   => Handle adress m
   -> adress
   -> m ServerStatus
getStatus Handle{..} adress = do
   logInfo hLogger $ "Scanning " .< adress 
   status <- hGetStatus adress
   logInfo hLogger $ "Status: " .< status
   return status