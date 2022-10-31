{-# LANGUAGE RecordWildCards #-}
-- | Module difenes bot logic.
module Status.Handle
    ( Handle(..)
    , ServerAdress(..)
    , ServerStatus(..)
    ) where

import Data.ByteString (ByteString)

import qualified Logger

-- | Handle to inject imlementation into logic.
data Handle m = Handle
   { hLogger :: Logger.Handle m
   -- ^ Injected logger handle.
   , hGetStatus :: ServerAdress -> m ServerStatus
   -- ^ Logic is abstracted from actual
   -- implementation of this function.
   }

data ServerAdress = ServerAdress
   { _host :: ByteString
   , _port :: Int
   } deriving (Show)

data ServerStatus
   = Online
   | NotAvaible
   deriving (Show, Eq)