{-# LANGUAGE DefaultSignatures #-}
-- | Printing data for humans.
module PrettyPrint
   ( PrettyPrint(..)
   ) where

import Data.Text (Text, pack)

-- | Class to unify all user interface data
-- printing.
class PrettyPrint a where
   -- | Converts given data to Text.
   -- You probably want to define resulted text
   -- in human-readable form
   prettyPrint :: a -> Text
   -- | Default implementation for debugging/scatches.
   default prettyPrint :: Show a => a -> Text
   prettyPrint = pack . show