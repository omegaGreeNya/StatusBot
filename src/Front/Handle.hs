{-# LANGUAGE GADTs           #-}
{-# LANGUAGE RecordWildCards #-}
-- | 
module Front.Handle
   ( Handle(..)
   , runFrontOnce
   ) where

import Control.Monad.IO.Class (MonadIO)

import Status.Handle (ServerStatus)
import Logger (logInfo)
import Utils ((.<),(>.))

import qualified Status.Handle as Status
import qualified Logger

-- | Handle provides logger, and to front functions
-- to get commands and to send answers.
data Handle user adress m = Handle 
   { hLogger      :: Logger.Handle m
   , hStatus      :: Status.Handle adress m
   , hGetAdresses :: m [(user, adress)]
   -- ^ Commands may come from different sources,
   -- it can be direct user command or some daily routine.
   -- This function incapsulates that process.
   , hSendAnswer  :: user -> adress -> ServerStatus -> m ()
   -- ^ Sends servers status to user.
   }

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