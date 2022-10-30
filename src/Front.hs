{-# LANGUAGE RecordWildCards #-}
-- | Module defines generic front use.
module Front
   ( Handle
   , runFrontOnce
   ) where

import Control.Monad.IO.Class (MonadIO)

import Status (ServerStatus)
import Logger (logInfo)
import Utils ((.<),(>.))

import Front.Handle (Handle(..))
import qualified Logger
import qualified Status

-- | Asks for adresses, makes servers status calls,
-- sends results back to users.
-- This function intented to be used repeatedly.
runFrontOnce
   :: (MonadIO m, Show user, Show adress)
   => Handle user adress m
   -> m ()
runFrontOnce h@Handle{..} = do
   (users, adresses) <- fmap unzip $ getCommands h
   
   statuses <- mapM (Status.getStatus hStatus) adresses
   
   mapM_ (sendAnswers h) $ zip3 users adresses statuses

-- << Implementation

getCommands
   :: (MonadIO m, Show user, Show adress)
   => Handle user adress m
   -> m [(user, adress)]
getCommands Handle{..} = do
   commands <- hGetAdresses
   mapM_ (uncurry (logCommand hLogger)) commands
   return commands

logCommand 
   :: (Show user, Show adress) 
   => Logger.Handle m
   -> user
   -> adress
   -> m ()
logCommand hLogger user cmd = logInfo hLogger
   $ user >. " asked for " .< cmd

sendAnswers
   :: Handle user adress m
   -> (user, adress, ServerStatus)
   -> m ()
sendAnswers Handle{..} (user, adress, status) =
   hSendAnswer user adress status

-- >>