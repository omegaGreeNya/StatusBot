-- | Moodule defines implementation of status handle functions,
-- and exports it's interface.
module Status
    ( Handle
    , Status(..)
    , ServerAdress
    , createHandle
    , getStatus
    ) where

import Control.Monad.IO.Class (MonadIO, liftIO)
import Data.Text (Text)

import Status.Handle (Status(..))

import qualified Logger (Handle)
import qualified Status.Handle as H (Handle(..), getStatus)

type Handle m = H.Handle ServerAdress m

type ServerAdress = Text

-- | Creates handle implementation.
createHandle :: MonadIO m => Logger.Handle m -> Handle m
createHandle logger = H.Handle logger getStatusRaw

-- | Returns status of provided server adress.
getStatus 
   :: (Monad m)
   => Handle m
   -> ServerAdress
   -> m Status
getStatus = H.getStatus

-- | Returns status of provided server adress.
getStatusRaw :: MonadIO m => ServerAdress -> m Status
getStatusRaw = undefined
