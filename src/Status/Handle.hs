{-# LANGUAGE RecordWildCards #-}
-- | Module difenes bot logic.
module Status.Handle
    ( Handle(..)
    , Status(..)
    , getStatus
    ) where

import Logger (logInfo)
import Utils ((.<))

import qualified Logger

-- | Handle to inject imlementation into logic.
data Handle adress m = Handle
   { hLogger :: Logger.Handle m
   -- ^ Injected logger handle.
   , hGetStatus :: adress -> m Status
   -- ^ Logic is abstracted from actual
   -- implementation of this function.
   }

data Status
   = Online
   | Offline
   deriving (Show, Eq)

-- | Returns status and logs action.
getStatus
   :: (Monad m, Show adress)
   => Handle adress m
   -> adress
   -> m Status
getStatus Handle{..} adress = do
   logInfo hLogger $ "Scanning " .< adress 
   status <- hGetStatus adress
   logInfo hLogger $ "Status: " .< status
   return status