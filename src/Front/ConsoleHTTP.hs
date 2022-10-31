{-# LANGUAGE RecordWildCards #-}
-- | Front implementation.
module Front.ConsoleHTTP
   ( ConsoleHTTP
   , createHandleWithProvidedInput
   , createHandle
   ) where

import Control.Monad.IO.Class (MonadIO, liftIO)
import Data.Text (Text)

import qualified Data.Text.IO as T

import qualified Front.Handle as Front
import qualified Logger.Handle as Logger


data ConsoleHTTP = ConsoleHTTP
   deriving (Show)

type Handle m = Front.Handle ConsoleHTTP m

createHandleWithProvidedInput
   :: MonadIO m => Logger.Handle m -> Text -> Handle m
createHandleWithProvidedInput hLogger msg =
   let hGetMessages = return [(ConsoleHTTP, msg)]
       hSendMessage _ text = liftIO $ T.putStrLn text
   in Front.Handle{..}

createHandle :: MonadIO m => Logger.Handle m -> Handle m
createHandle hLogger =
   let hGetMessages = getMessages
       hSendMessage _ text = liftIO $ T.putStrLn text
   in Front.Handle{..}

getMessages :: MonadIO m => m [(ConsoleHTTP, Text)]
getMessages = do
   msg <- liftIO T.getLine
   return [(ConsoleHTTP, msg)]