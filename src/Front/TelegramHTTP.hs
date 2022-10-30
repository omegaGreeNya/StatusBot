{-# LANGUAGE RecordWildCards   #-}
-- | Front implementation.

-- TO-DO
-- Add user input validation.
module Front.TelegramHTTP
   ( createHandle
   ) where

import Control.Monad.IO.Class (MonadIO)
import Data.Text (Text)

import qualified Data.ByteString.Char8 as BS
import qualified Data.Text.Encoding as T

import API.Telegram (TelegramUser)
import Status (ServerAdress(..), ServerStatus)
import Utils ((.<),(>.))

import qualified API.Telegram as API
import qualified Front.Handle as Front
import qualified Status
import qualified Logger.Handle as Logger


type Handle m = Front.Handle TelegramUser ServerAdress m

createHandle :: MonadIO m => API.Handle m -> Logger.Handle m -> Handle m
createHandle hAPI hLogger =
   let hStatus = Status.createHandle hLogger
       hGetAdresses = pollAdresses hAPI
       hSendAnswer = sendAnswer hAPI
   in Front.Handle{..}

-- << Implementation

sendAnswer :: MonadIO m => API.Handle m -> TelegramUser -> ServerAdress -> ServerStatus -> m ()
sendAnswer hAPI chat_id adress status = do
   let msg = adress >. " is " .< status <> "."
   _ <- API.sendAnswer hAPI chat_id msg
   return ()

-- | Makes polling call to telegram api
-- and returns users server status requests.
pollAdresses :: MonadIO m => API.Handle m -> m [(TelegramUser, ServerAdress)]
pollAdresses hAPI = do
   json <- API.getUpdates hAPI
   return $ parseUpdates json

-- | Parses raw json list of telegram Update to users calls.
parseUpdates :: BS.ByteString -> [(TelegramUser, ServerAdress)]
parseUpdates json = let
    (users, messages) = unzip $ API.parseMessageUpdates json
    in zip users (map parseAdress messages)

parseAdress :: Text -> ServerAdress
parseAdress text = let
   host = (T.encodeUtf8 text)
   in ServerAdress {_host = host, _port = 80}