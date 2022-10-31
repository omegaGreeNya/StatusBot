{-# LANGUAGE RecordWildCards #-}
-- | Module defines generic front use.
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