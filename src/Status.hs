{-# LANGUAGE RecordWildCards #-}
-- | Moodule defines implementation of status handle functions,
-- and exports it's interface.
module Status
    ( Handle
    , ServerStatus(..)
    , ServerAdress(..)
    , createHandle
    , getStatus
    , getStatusRaw
    ) where

import Control.Exception (try)
import Control.Monad.IO.Class (MonadIO, liftIO)
import Data.ByteString (ByteString)
import Network.HTTP.Client (HttpException)
import Network.HTTP.Simple
   ( Request, httpNoBody, getResponseStatus
   , setRequestHost, setRequestPort, defaultRequest)
import Network.HTTP.Types (Status(..))

import Status.Handle (ServerStatus(..))

import qualified Logger (Handle)
import qualified Status.Handle as H (Handle(..), getStatus)

type Handle m = H.Handle ServerAdress m

data ServerAdress = ServerAdress
   { _host :: ByteString
   , _port :: Int
   } deriving (Show)

-- << Interface

-- | Creates handle implementation.
createHandle :: MonadIO m => Logger.Handle m -> Handle m
createHandle logger = H.Handle logger getStatusRaw

-- | Returns status of provided server adress.
getStatus 
   :: (Monad m)
   => Handle m
   -> ServerAdress
   -> m ServerStatus
getStatus = H.getStatus
-- >>

-- << Implementation

-- | Returns status of provided server adress.
getStatusRaw :: MonadIO m => ServerAdress -> m ServerStatus
getStatusRaw adress = do
   let request = constructPingRequest adress
   eResponse <- liftIO . try $ httpNoBody request
   return $ case eResponse of
      Left e -> httpExceptionToServerStatus e
      Right response -> fromStatus $ getResponseStatus response

httpExceptionToServerStatus :: HttpException -> ServerStatus
httpExceptionToServerStatus _ = NotAvaible

fromStatus :: Status -> ServerStatus
fromStatus status =
   case statusCode status of
      200 -> Online
      _   -> NotAvaible

constructPingRequest :: ServerAdress -> Request
constructPingRequest ServerAdress{..} =
   setRequestHost _host
   $ setRequestPort _port
   $ defaultRequest
-- >>