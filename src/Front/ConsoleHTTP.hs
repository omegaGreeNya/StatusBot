{-# LANGUAGE RecordWildCards #-}
-- | Console front implementation.
module Front.ConsoleHTTP
   ( createHandle
   ) where

import Control.Monad.IO.Class (MonadIO, liftIO)
import Data.Text (Text)

import qualified Data.Text.IO as T

import qualified Front.Handle as Front
import qualified Logger as Logger

-- | Console has only one user called ConsoleHTTP.
data ConsoleHTTP = ConsoleHTTP
   deriving (Show)

type Handle m = Front.Handle ConsoleHTTP m

-- | Creates Console front handle.
createHandle :: MonadIO m => Logger.Handle m -> Handle m
createHandle hLogger =
   let hGetMessages = getMessages
       hSendMessage _ text = liftIO $ T.putStrLn text
   in Front.Handle{..}

-- | Packs console input line with ConsolleHTTP
-- as message from console front.
getMessages :: MonadIO m => m [(ConsoleHTTP, Text)]
getMessages = do
   msg <- liftIO T.getLine
   return [(ConsoleHTTP, msg)]