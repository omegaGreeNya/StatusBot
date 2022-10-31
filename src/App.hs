{-# LANGUAGE RecordWildCards #-}
-- | App behavior defined here.
module App
   ( Handle(..)
   , runAppSimpleForever
   , runAppSimple
   ) where

import Control.Concurrent (threadDelay)
import Control.Monad (forever)
import Control.Monad.IO.Class (MonadIO, liftIO)
import Data.Text (Text)

import PrettyPrint
import Parsing (parseAddress)

import qualified Front as Front
import qualified Logger as Logger
import qualified Status as Status

data Handle user m = Handle
   { hFront  :: Front.Handle user m
   , hLogger :: Logger.Handle m 
   , hStatus :: Status.Handle m
   }

-- | Runs app in forever loop.
runAppSimpleForever
   :: (MonadIO m, Show user)
   => Handle user m
   -> m ()
runAppSimpleForever h = forever $ runAppSimple h

runAppSimple
   :: (MonadIO m, Show user)
   => Handle user m
   -> m ()
runAppSimple h@Handle{..} = do
   userMessages <- Front.getMessages hFront
   mapM_ (uncurry (processUserMessage h)) userMessages
   liftIO $ threadDelay 100000 -- 0.1 sec

processUserMessage
   :: (MonadIO m, Show user)
   => Handle user m
   -> user
   -> Text
   -> m ()
processUserMessage Handle{..} user text = do
   let eAdress = parseAddress text
   case eAdress of
      Left parseErr -> 
         Front.sendMessage hFront user
            $ prettyPrint parseErr
      Right address -> do
         serverStatus <- Status.getStatus hStatus address
         Front.sendMessage hFront user
            $ "Server " 
            <> (prettyPrint address)
            <> " is "
            <> (prettyPrint serverStatus)
