{-# LANGUAGE RecordWildCards #-}
-- | Different funtions to make life easier.
module Parsing
   ( parseAddress
   ) where


import Data.ByteString (ByteString)
import Data.Text (Text)
import Network.HTTP.Client (isIpAddress)

import qualified Data.Text.Encoding as T

import PrettyPrint
import Status (ServerAdress(..))

data ParsingError 
   = AdressParsingError
      { parsedText   :: Text
      , errorMessage :: Text
      }
   -- | WhateverParsingError
   deriving (Show)

instance PrettyPrint ParsingError where
   prettyPrint AdressParsingError{..} = errorMessage


parseAddress
   :: Text
   -> Either ParsingError ServerAdress
parseAddress text =
   let adress = T.encodeUtf8 text
   in if isIpAddress adress
      then Right $ makeHttpAdress adress
      else Left $ AdressParsingError text
                $ "\"" <> text <> "\" is not a valid IP address."

makeHttpAdress :: ByteString -> ServerAdress
makeHttpAdress host =
   ServerAdress {_host = host, _port = 80}