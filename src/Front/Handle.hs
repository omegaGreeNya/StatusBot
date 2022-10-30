-- | Inner module, defines front handle.
module Front.Handle
   ( Handle(..)
   ) where

import Status.Handle (ServerStatus)

import qualified Status.Handle as Status
import qualified Logger

-- | Handle provides logger, and to front functions
-- to get commands and to send answers.
data Handle user adress m = Handle 
   { hLogger      :: Logger.Handle m
   , hStatus      :: Status.Handle adress m
   , hGetAdresses :: m [(user, adress)]
   -- ^ Commands may come from different sources,
   -- it can be direct user command or some daily routine.
   -- This function incapsulates that process.
   , hSendAnswer  :: user -> adress -> ServerStatus -> m ()
   -- ^ Sends servers status to user.
   }