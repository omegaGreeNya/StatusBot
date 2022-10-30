{-# LANGUAGE RecordWildCards #-}
-- | Moodule defines implementation of status handle functions.
module Status.Implementation
    ( Handle
    , ServerAdress(..)
    , ServerStatus(..)
    , createHandle
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
import qualified Status.Handle as H (Handle(..))


type Handle m = H.Handle ServerAdress m

data ServerAdress = ServerAdress
   { _host :: ByteString
   , _port :: Int
   } deriving (Show)


-- | Creates handle implementation.
createHandle :: MonadIO m => Logger.Handle m -> Handle m
createHandle logger = H.Handle logger getStatusRaw

-- << Implementation

-- | Returns status of provided server adress.
getStatusRaw :: MonadIO m => ServerAdress -> m ServerStatus
getStatusRaw adress = do
   let request = constructPingRequest adress
   eResponse <- liftIO . try $ httpNoBody request
   return $ case eResponse of
      Left e -> httpExceptionToServerStatus e
      Right response -> fromStatus $ getResponseStatus response

-- | Translates Http exception into server status.
httpExceptionToServerStatus :: HttpException -> ServerStatus
httpExceptionToServerStatus _ = NotAvaible

-- | Translates Http response status into server status.
fromStatus :: Status -> ServerStatus
fromStatus status
   | code == 200 = Online
   | otherwise   = NotAvaible
   where
    code = statusCode status

-- | Translates adress into http request.
constructPingRequest :: ServerAdress -> Request
constructPingRequest ServerAdress{..} =
   setRequestHost _host
   $ setRequestPort _port
   $ defaultRequest
-- >>