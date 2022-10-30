{-# LANGUAGE RecordWildCards #-}
-- | Module difenes bot logic.
module Status.Handle
    ( Handle(..)
    , ServerStatus(..)
    ) where

import Logger (logInfo)
import Utils ((.<))

import qualified Logger

-- | Handle to inject imlementation into logic.
data Handle adress m = Handle
   { hLogger :: Logger.Handle m
   -- ^ Injected logger handle.
   , hGetStatus :: adress -> m ServerStatus
   -- ^ Logic is abstracted from actual
   -- implementation of this function.
   }

data ServerStatus
   = Online
   | NotAvaible
   deriving (Show, Eq)