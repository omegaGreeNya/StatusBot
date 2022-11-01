{-# LANGUAGE RecordWildCards #-}
-- | Moodule defines Status calls interface for
-- buisness logic/mid-layer of the app.
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