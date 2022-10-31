{-# LANGUAGE RecordWildCards   #-}
-- | Front implementation.
module API.Telegram
   ( Handle (..)
   , TelegramUser
   , createHandle
   , getUpdates
   , parseMessageUpdates
   , sendMessage
   ) where

import Control.Lens (toListOf)
import Control.Monad.IO.Class (MonadIO, liftIO)
import Data.Aeson.Lens (_Array, _Integer, _String, key)
import Data.ByteString.Char8 (ByteString)
import Data.IORef (newIORef, readIORef, writeIORef)
import Data.Text (Text)
import Network.HTTP.Simple 
   ( Response, Request, defaultRequest, getResponseBody
   , setRequestMethod, setRequestHost, setRequestSecure
   , setRequestPath, setRequestPort, setRequestQueryString)

import qualified Data.ByteString.Char8 as BS
import qualified Data.Text.Encoding as T

import Utils (packQVal)
import Utils.Exceptions (safeHttpBS)

import qualified Logger

data Handle m = Handle
   { hLogger    :: Logger.Handle m
   , hToken     :: Text
   , hTimeout   :: Int
   , hGetOffset :: m Int
   , hSetOffset :: Int -> m ()
   }

type TelegramUser = Int

-- << Interface

-- | Creates simple Handle with IORef holding current offset.
createHandle :: MonadIO m => Logger.Handle m -> Text -> Int -> m (Handle m)
createHandle hLogger hToken hTimeout = do
   offsetRef <- liftIO $ newIORef 0
   let hGetOffset = liftIO $ readIORef offsetRef
   let hSetOffset offset = liftIO $ writeIORef offsetRef offset
   return Handle{..}

-- | Gets updates from telegram API.
-- https://core.telegram.org/bots/api#getupdates
getUpdates :: MonadIO m => Handle m -> m (Maybe BS.ByteString)
getUpdates h@Handle{..} = do
   request <- getUpdatesRequest h
   mRes <- safeHttpBS hLogger request
   case mRes of 
      Nothing -> return Nothing
      Just res -> do
      let response = getResponseBody res
      updateOffset h response
      return $ Just response

-- | Parses raw json list of telegram Update
-- to users and their message texts.
parseMessageUpdates :: BS.ByteString -> [(TelegramUser, Text)]
parseMessageUpdates json = let
   messages = toListOf (key "result" . _Array . traverse . key "message") json
   
   users = map fromIntegral 
         $ toListOf (traverse . key "chat" . key "id" . _Integer) messages
   
   adresses = toListOf (traverse . key "text" . _String) messages
   
   in zip users adresses

-- | Posts message to given chat_id.
-- Returns posted message.
-- https://core.telegram.org/bots/api#sendmessage
sendMessage :: MonadIO m => Handle m -> TelegramUser -> Text -> m (Maybe (Response ByteString))
sendMessage h@Handle{..} chat_id msg = do
   let request = sendMessageRequest h chat_id msg
   safeHttpBS hLogger request


-- >>

-- << Implementation

-- | Updates offset with raw response from getUpdates.
updateOffset :: Handle m -> BS.ByteString -> m ()
updateOffset Handle{..} json =
   hSetOffset $ getOffset json

-- | Returns biggest telegram update_id in response on getUpdates.
-- If parsing fails, returns 1 as last update_id.
getOffset :: BS.ByteString -> Int
getOffset json =
   let ids = toListOf
            (key "result" . _Array . traverse . key "update_id" . _Integer)
            json
   in (+1) . foldl max 1 $ map fromIntegral ids
   -- "+ 1", since telegram sends all updates with ids greated than
   -- given offset, and since we doesn't want to get same update twice
   -- or more times, we increase greates by one.

-- <<< Requests

-- | Template request.
telegramRequest :: Handle m -> ByteString -> Request
telegramRequest Handle{..} method = setRequestSecure True
   $ setRequestPort 443
   $ setRequestHost "api.telegram.org"
   $ setRequestPath ("/bot" <> (T.encodeUtf8 hToken) <> "/" <> method)
   $ defaultRequest

-- | Telegram sendMessage method request.
sendMessageRequest :: Handle m -> TelegramUser -> Text -> Request
sendMessageRequest h chat_id msg  =
   setRequestMethod "POST" -- actually not necessary for telegram
   $ setRequestQueryString
      [ ("chat_id", packQVal chat_id)
      , ("disable_web_page_preview", packQVal True)
      , ("text", packQVal msg)]
   $ telegramRequest h "sendMessage"

-- | Telegram getUpdates method request.
getUpdatesRequest :: Monad m => Handle m -> m Request
getUpdatesRequest h@Handle{..} = do
   offset <- hGetOffset
   return 
      $ setRequestQueryString
         [ ("offset", packQVal offset)
         , ("timeout", packQVal hTimeout)]
      $ telegramRequest h "getUpdates"
-- >>>
-- >>