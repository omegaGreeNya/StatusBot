{-# LANGUAGE DefaultSignatures #-}
-- | Different funtions to make life easier.
module Utils.Exceptions
   ( safeHttpBS
   ) where

import Control.Exception (try)
import Control.Monad.IO.Class (MonadIO, liftIO)

import Data.ByteString (ByteString)
import Network.HTTP.Simple
   (HttpException(..), Response, Request, httpBS)
import Logger (logWarning, logError)

import qualified Data.Text as T

import Utils ((.<))

import qualified Logger

-- << HTTP

safeHttpBS 
   :: MonadIO m
   => Logger.Handle m
   -> Request
   -> m (Maybe (Response ByteString))
safeHttpBS hLogger req = do
   eRes <- liftIO . try $ httpBS req
   case eRes of
      Left httpErr -> do
         handleHttpException hLogger httpErr
         return Nothing
      Right res -> return $ Just res

handleHttpException
   :: Logger.Handle m
   -> HttpException
   -> m ()
handleHttpException hLogger err@(InvalidUrlException _ _) = 
   logError hLogger $ T.pack (show err)
handleHttpException hLogger err@(HttpExceptionRequest _ _) =
   logWarning hLogger $ "Request Failed: " .< err

-- >>