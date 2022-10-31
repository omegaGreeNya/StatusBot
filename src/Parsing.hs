{-# LANGUAGE RecordWildCards #-}
-- | Different funtions to make life easier.
module Parsing
   ( parseAddress
   ) where


import Data.ByteString (ByteString)
import Data.Text (Text)

import qualified Data.Text as T
import qualified Data.Text.Encoding as T

import PrettyPrint
import Status (ServerAdress(..))

data ParsingError 
   = AdressParsingError Text
   -- | WhateverParsingError
   deriving (Show)

instance PrettyPrint ParsingError where
   prettyPrint (AdressParsingError parsedText) = 
      "\"" <> parsedText <> "\" can't be parse as site or ip." 


parseAddress
   :: Text
   -> Either ParsingError ServerAdress
parseAddress text =
   let textStripped = T.strip text
       adress = T.encodeUtf8 textStripped
   in case adress of
      "" -> Left $ AdressParsingError text
      _  -> Right $ makeHttpAdress adress
-- If adress is actually malformed, that would leak
-- to Status logic, and then caught on http call.
-- Not a big deal in the end of the day, but
-- sure thing to consider in future.

makeHttpAdress :: ByteString -> ServerAdress
makeHttpAdress host =
   ServerAdress {_host = host, _port = 80}