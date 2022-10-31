-- | Inner module, defines front handle.
module Front.Handle
   ( Handle(..)
   ) where

import Data.Text (Text)

import qualified Logger

-- | Handle provides logger, and to front functions
-- to get commands and to send answers.
data Handle user m = Handle 
   { hLogger      :: Logger.Handle m
   , hGetMessages :: m [(user, Text)]
   -- ^ Method to get user input.
   , hSendMessage :: user -> Text -> m ()
   -- ^ Method to send output to user.
   }