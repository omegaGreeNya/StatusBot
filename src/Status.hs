{-# LANGUAGE RecordWildCards #-}
-- | Moodule defines implementation of status handle functions.

-- Move parsing in separate module?
module Status
    ( Handle
    , ServerAdress(..)
    , ServerStatus(..)
    , getStatus
    ) where

import Logger (logInfo)
import Status.Handle (Handle(..), ServerAdress(..), ServerStatus(..))
import Utils ((.<))

-- | Returns status and logs action.
getStatus
   :: (Monad m)
   => Handle m
   -> ServerAdress
   -> m ServerStatus
getStatus Handle{..} adress = do
   logInfo hLogger $ "Scanning " .< adress 
   status <- hGetStatus adress
   logInfo hLogger $ "Status: " .< status
   return status