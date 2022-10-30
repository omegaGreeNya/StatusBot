{-# LANGUAGE RecordWildCards #-}
-- | Front implementation.

-- TO-DO
-- Add user input validation.
module Front.ConsoleHTTP
   ( createHandle
   ) where

import Control.Monad.IO.Class (MonadIO, liftIO)
import Data.ByteString (ByteString)
import Data.Text (Text)

import qualified Data.Text.IO as T
import qualified Data.Text.Encoding as T

import Status (ServerAdress(..), ServerStatus)
import Utils ((.<),(>.))

import qualified Front.Handle as Front
import qualified Status
import qualified Logger.Handle as Logger


data ConsoleHTTP = ConsoleHTTP
   deriving (Show)

type Handle m = Front.Handle ConsoleHTTP ServerAdress m

createHandle :: MonadIO m => Logger.Handle m -> Handle m
createHandle hLogger =
   let hStatus = Status.createHandle hLogger
       hGetAdresses = liftIO T.getLine >>= parseInput hLogger
       hSendAnswer = printStatus
   in Front.Handle{..}

-- << Implementation

-- | Parses input command, may return empty list
parseInput :: MonadIO m => Logger.Handle m -> Text -> m [(ConsoleHTTP, ServerAdress)]
parseInput hLogger input = do
   mHost <- parseHost hLogger input
   return $ case mHost of
      Nothing -> []
      Just host -> [(ConsoleHTTP, ServerAdress {_host = host, _port = 80})]

-- | Parses text as ip or host.
-- Returns Nothing if parsing failed.
parseHost :: Monad m => Logger.Handle m -> Text -> m (Maybe ByteString)
parseHost _ text = return . Just $ T.encodeUtf8 text

-- | Prints given adress and astatus to console.
printStatus :: MonadIO m => ConsoleHTTP -> ServerAdress -> ServerStatus -> m ()
printStatus _ adress status = liftIO $
   T.putStrLn $ adress >. " is " .< status <> "."
-- >>