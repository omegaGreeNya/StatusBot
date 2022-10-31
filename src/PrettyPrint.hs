{-# LANGUAGE DefaultSignatures #-}
-- | Printing data for humans.
module PrettyPrint
   ( PrettyPrint(..)
   ) where

import Data.Text (Text, pack)

class PrettyPrint a where
   prettyPrint :: a -> Text
   default prettyPrint :: Show a => a -> Text
   prettyPrint = pack . show