{-# LANGUAGE RecordWildCards #-}
-- | App
module App
   ( Handle(..)
   , runAppSimpleForever
   ) where

import Control.Concurrent (threadDelay)
import Control.Monad (forever)
import Control.Monad.IO.Class (MonadIO, liftIO)
import Data.Text (Text)

import Utils ((.<))

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
runAppSimpleForever h@Handle{..} =
   forever $ do
      userMessages <- Front.getMessages hFront
      mapM_ (uncurry (processUserMessage h)) userMessages
      liftIO $ threadDelay 100000

processUserMessage
   :: (MonadIO m, Show user)
   => Handle user m
   -> user
   -> Text
   -> m ()
processUserMessage Handle{..} user text = do
   let eAdress = Status.parseAdress text
   case eAdress of
      Left parseErr -> 
         Front.sendMessage hFront user
            $ "" .< parseErr
      Right adress -> do
         serverStatus <- Status.getStatus hStatus adress
         Front.sendMessage hFront user
            $ "Server " .< adress <> " is " .< serverStatus