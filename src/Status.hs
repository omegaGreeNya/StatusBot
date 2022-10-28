-- | Moodule defines implementation of status handle functions.
module Status
    ( Handle
    , Status(..)
    , ServerAdress
    , createHandle
    , getStatus
    ) where

import Status.Handle (Status(..))
import qualified Status.Handle (Handle, getStatus) as H

type Handle m = H.Handle ServerAdress m

type ServerAdress = Text

-- | Creates handle implementation.
createHandle :: MonadIO m => Logger.Handle m -> Handle m
createHandle logger = Handle logger getStatusRaw

-- | Returns status of provided server adress.
getStatusRaw :: MonadIO m => ServerAdress -> m Status
getStatusRaw getStatus adress = undefined