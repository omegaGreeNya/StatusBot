{-# LANGUAGE RecordWildCards   #-}
-- | Front implementation.

-- TO-DO
-- Add user input validation.
module Front.TelegramHTTP
   ( createHandle
   ) where

import Control.Monad.IO.Class (MonadIO)
import Data.Text (Text)

import API.Telegram (TelegramUser)

import qualified API.Telegram as API
import qualified Front.Handle as Front
import qualified Logger.Handle as Logger


type Handle m = Front.Handle TelegramUser m

createHandle :: MonadIO m => API.Handle m -> Logger.Handle m -> Handle m
createHandle hAPI hLogger =
   let hGetMessages = pollMessages hAPI
       hSendMessage = sendMessage hAPI
   in Front.Handle{..}

-- << Implementation

sendMessage :: MonadIO m => API.Handle m -> TelegramUser -> Text -> m ()
sendMessage hAPI chat_id msg = do
   _ <- API.sendMessage hAPI chat_id msg
   return ()

-- | Makes polling call to telegram api
-- and returns users server status requests.
pollMessages :: MonadIO m => API.Handle m -> m [(TelegramUser, Text)]
pollMessages hAPI = do
   mJson <- API.getUpdates hAPI
   return $ case mJson of 
      Nothing   -> []
      Just json -> API.parseMessageUpdates json