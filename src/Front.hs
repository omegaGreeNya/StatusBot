{-# LANGUAGE RecordWildCards #-}
-- | Module defines front usage for mid-layer part of app.
module Front
   ( Handle
   , getMessages
   , sendMessage
   ) where

import Control.Monad.IO.Class (MonadIO)
import Data.Text (Text)

import Logger (logInfo)
import Utils ((.<),(>.))

import Front.Handle (Handle(..))

-- | Asks front for messages and returns resulted list
-- of users and their messages. Logs this action.
getMessages
   :: (MonadIO m, Show user)
   => Handle user m
   -> m [(user, Text)]
getMessages Handle{..} = do
   msgs <- hGetMessages
   mapM_ logMessage msgs
   return msgs
   where
      logMessage (user, msg) =
         logInfo hLogger
            $ user >. " sended: " <> msg

-- | Send given text to specified user. Logs this action.
sendMessage
   :: (MonadIO m, Show user)
   => Handle user m
   -> user
   -> Text
   -> m ()
sendMessage Handle{..} user msg = do
   logInfo hLogger
      $ "Sending \"" <> msg <> "\" to user: " .< user
   hSendMessage user msg