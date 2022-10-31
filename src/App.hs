{-# LANGUAGE RecordWildCards #-}
-- | App behavior defined here.
module App
   ( Handle(..)
   , runAppSimpleForever
   , runAppSimple
   , runAppAdmin
   ) where

import Control.Concurrent (threadDelay)
import Control.Monad (forever)
import Control.Monad.IO.Class (MonadIO, liftIO)
import Data.Text (Text)

import qualified Data.Text as T

import PrettyPrint
import Parsing (parseAddress)
import Utils ((>.))

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
   mapM_ (uncurry (processStatusCommand h)) userMessages
   liftIO $ threadDelay 100000 -- 0.1 sec

-- | Runs app in special - admin mode
runAppAdmin
   :: (MonadIO m, Show user)
   => Handle user m
   -> m ()
runAppAdmin h@Handle{..} = do
   userMessages <- Front.getMessages hFront
   let processMessages [] = runAppAdmin h
       processMessages ((user, message):msgs) = do
         case T.words message of
            ("stop":_)
               -> return ()
            ("help":_)
               -> sendHelp h user >> processMessages msgs 
            ("getStatus":rest)
               -> processStatusCommand h user (T.unwords rest)
               >> processMessages msgs 
            _
               -> sendHint h user message
               >> processMessages msgs
   processMessages userMessages

-- | Answers to given user on supplied text
-- as on serverStatus command. Informs user if
-- IP is malformed.
processStatusCommand
   :: (MonadIO m, Show user)
   => Handle user m
   -> user
   -> Text
   -> m ()
processStatusCommand Handle{..} user text = do
   let eAdress = parseAddress text
   case eAdress of
      Left parseErr -> 
         Front.sendMessage hFront user
            $ prettyPrint parseErr
      Right address -> do
         serverStatus <- Status.getStatus hStatus address
         Front.sendMessage hFront user
            $ (prettyPrint address)
            <> " is "
            <> (prettyPrint serverStatus)

sendHint
   :: (MonadIO m, Show user)
   => Handle user m
   -> user
   -> Text
   -> m ()
sendHint Handle{..} user unknownCommand =
   Front.sendMessage hFront user
      $ unknownCommand >. " is not valid command, use \"help\" to get commands list"

sendHelp
   :: (MonadIO m, Show user)
   => Handle user m
   -> user
   -> m ()
sendHelp Handle{..} user =
   Front.sendMessage hFront user
      $ T.unlines
      [ "help - prints this message"
      , "stop - to stop app. Note, it may take time to close http calls."
      , "getStatus <IP> - asks for server status."
      ]