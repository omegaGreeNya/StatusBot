{-# LANGUAGE DefaultSignatures #-}
-- | Different funtions to make life easier.
module Utils
   ( derivingDrop
   
   , getCurrentTimeHiRes
   
   , packQVal
   
   , (.<~)
   , (~>.)
   , (.<)
   , (>.)
   ) where

import Control.Monad.IO.Class (MonadIO, liftIO)
import Data.Aeson.TH (Options (..), defaultOptions)
import Data.Text (Text)
import Data.Time.Clock.System (getSystemTime)

import qualified Data.ByteString.Char8 as BS8
import qualified Data.Text as T
import qualified Data.Text.Encoding as T

-- << Text manipulations / Formating
(.<~) :: Show a => Text -> a -> Text
(.<~) text a = text <> "\"" <> (T.pack $ show a) <> "\""
infixr 7 .<~ 
(~>.) :: Show a => a -> Text -> Text
(~>.) a text = "\"" <> (T.pack $ show a) <> "\"" <> text
infixr 7 ~>.
(.<) :: Show a => Text -> a -> Text
(.<) text a = text <> (T.pack $ show a)
infixr 7 .<
(>.) :: Show a => a -> Text -> Text
(>.) a text = (T.pack $ show a) <> text
infixr 7 >.
-- <> has precedence infixr 6. 
-- And we define infixr 7 for [.<, ..] since we want replace them to <>, and then concatinate
-- >>

-- << Aeson

-- | Options to drop given amount of symbols
-- in field name.
-- Due TH specific, we can't use template 
-- with functions defined in same module.
derivingDrop :: Int -> Options
derivingDrop n = defaultOptions {fieldLabelModifier = drop n}

-- >>

-- << Hi-Res Time

getCurrentTimeHiRes :: MonadIO m => m Int
getCurrentTimeHiRes = undefined

-- >>

-- << 

-- | Type class for easy, dead-brain encoding ByteStrings
class ToByteString a where
   toBS :: a -> BS8.ByteString
   default toBS :: Show a => a -> BS8.ByteString
   toBS = BS8.pack . show

instance ToByteString Int
instance ToByteString Bool
instance ToByteString Text where
   toBS = T.encodeUtf8

-- | Helper function for packing query items.
packQVal :: ToByteString a => a -> Maybe BS8.ByteString
packQVal a = Just $ toBS a

-- >>