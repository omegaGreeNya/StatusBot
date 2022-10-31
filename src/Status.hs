{-# LANGUAGE RecordWildCards #-}
-- | Moodule defines implementation of status handle functions.

-- Move parsing in separate module?
module Status
    ( Handle
    , ServerAdress(..)
    , ServerStatus(..)
    , getStatus
    , parseAdress
    ) where

import Data.ByteString (ByteString)
import Data.Text (Text)
import Network.HTTP.Client (isIpAddress)

import qualified Data.Text.Encoding as T

import Logger (logInfo)
import Status.Handle (Handle(..), ServerAdress(..), ServerStatus(..))
import Utils ((.<))

data ParsingError = AdressParsingError
   { parsedText   :: Text
   , errorMessage :: Text
   } deriving (Show)

-- | Returns status and logs action.
getStatus
   :: (Monad m)
   => Handle m
   -> ServerAdress
   -> m ServerStatus
getStatus Handle{..} adress = do
   logInfo hLogger $ "Scanning " .< adress 
   status <- hGetStatus adress
   logInfo hLogger $ "Status: " .< status
   return status

parseAdress
   :: Text
   -> Either ParsingError ServerAdress
parseAdress text =
   let adress = T.encodeUtf8 text
   in if isIpAddress adress
      then Right $ Status.makeHttpAdress adress
      else Left $ AdressParsingError text "Not a valid IP address."

makeHttpAdress :: ByteString -> ServerAdress
makeHttpAdress host =
   ServerAdress {_host = host, _port = 80}