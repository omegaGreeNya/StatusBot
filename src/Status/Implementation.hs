{-# LANGUAGE RecordWildCards #-}
-- | Moodule provides implementation of Status.Handle,
-- actual Handle Data Constructor hided behind createHandle.
module Status.Implementation
   ( Handle
   , ServerAdress(..)
   , ServerStatus(..)
   , createHandle
   ) where

import Control.Exception (try)
import Control.Monad.IO.Class (MonadIO, liftIO)
import Data.Text (Text)
import Network.HTTP.Client (HttpException(..), HttpExceptionContent(..))
import Network.HTTP.Simple
   (Request, httpNoBody, getResponseStatus, setRequestHost, setRequestPort, defaultRequest)
import Network.HTTP.Types (Status(..))

import qualified Data.Text as T

import Logger (logDebug, logError)
import Status.Handle (Handle(..), ServerAdress(..), ServerStatus(..))
import Utils ((.<))

import qualified Logger (Handle)

-- | Creates handle implementation.
createHandle :: MonadIO m => Logger.Handle m -> Text -> Handle m
createHandle hLogger feedbackAddress = Handle hLogger (getStatusRaw hLogger feedbackAddress)

-- << Implementation

-- | Returns status of provided server adress.
getStatusRaw :: MonadIO m => Logger.Handle m -> Text -> ServerAdress -> m ServerStatus
getStatusRaw hLogger feedbackAddress adress = do
   let request = constructPingRequest adress
   eResponse <- liftIO . try $ httpNoBody request
   case eResponse of
      Left e -> httpExceptionToServerStatus hLogger feedbackAddress e
      Right response -> return $ fromStatus $ getResponseStatus response

-- | Translates Http exception into server status.
httpExceptionToServerStatus
   :: Monad m => Logger.Handle m -> Text -> HttpException -> m ServerStatus
httpExceptionToServerStatus _ _ (InvalidUrlException _ msg) =
   return $ NotAvaible $ T.pack msg
httpExceptionToServerStatus hLogger feedbackAddress exception@(HttpExceptionRequest _ err) = do
   logDebug hLogger (T.pack $ show exception)
   case err of
      ConnectionFailure _
         -> return . NotAvaible $ "connection failure (Address is probably malformed)"
      ConnectionTimeout
         -> return . NotAvaible $ "connection timeout"
      ResponseTimeout
         -> return . NotAvaible $ "response timeout"
      TooManyRedirects _
         -> return . NotAvaible $ "too many redirects"
      _  -> do
         logError hLogger (T.pack $ show exception)
         return . NotAvaible $ "Internal bot error, please contact " <> feedbackAddress
      

-- | Translates Http response status into server status.
fromStatus :: Status -> ServerStatus
fromStatus status
   | code == 200 = Online
   | otherwise   = NotAvaible ("code: " .< code)
   where
    code = statusCode status

-- | Translates adress into http request.
constructPingRequest :: ServerAdress -> Request
constructPingRequest ServerAdress{..} =
   setRequestHost _host
   $ setRequestPort _port
   $ defaultRequest
-- >>