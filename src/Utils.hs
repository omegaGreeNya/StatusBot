{-# LANGUAGE DefaultSignatures #-}
-- | Different funtions to make life easier.
-- Or across-app functions what present not much of functional
-- to be presented in separate module.
module Utils
   ( derivingDrop
   
   , packQVal
   
   , (.<~)
   , (~>.)
   , (.<)
   , (>.)
   ) where

import Data.Aeson.TH (Options (..), defaultOptions)
import Data.Text (Text)

import qualified Data.ByteString.Char8 as BS8
import qualified Data.Text as T
import qualified Data.Text.Encoding as T

-- << Text manipulations / Formating

-- | Packs rigth-side value to Text and surronds it with " symbol.
-- After that, acts like '(<>)'.
(.<~) :: Show a => Text -> a -> Text
(.<~) text a = text <> "\"" <> (T.pack $ show a) <> "\""
infixr 7 .<~ 
-- | Packs left-side value to Text and surronds it with " symbol.
-- After that, acts like '(<>)'.
(~>.) :: Show a => a -> Text -> Text
(~>.) a text = "\"" <> (T.pack $ show a) <> "\"" <> text
infixr 7 ~>.
-- | Packs rigth-side value to Text and then acts like '(<>)'.
(.<) :: Show a => Text -> a -> Text
(.<) text a = text <> (T.pack $ show a)
infixr 7 .<
-- | Packs left-side value to Text and then acts like '(<>)'.
(>.) :: Show a => a -> Text -> Text
(>.) a text = (T.pack $ show a) <> text
infixr 7 >.
-- (<>) has precedence infixr 6. 
-- We define infixr 7 for [(.<), (.<~), ..] since we 
-- want replace them to packs and <>, and after that concatinate.
-- >>

-- << Aeson

-- | Options to drop given amount of symbols
-- in field name.
-- Due TH specific, we can't use template 
-- with functions defined in same module.
-- So derivingDrop defined here.
derivingDrop :: Int -> Options
derivingDrop n = defaultOptions {fieldLabelModifier = drop n}

-- >>

-- << HTTP Queries

-- | Type class to unify data to ByteString encoding.
class ToByteString a where
   toBS :: a -> BS8.ByteString
   default toBS :: Show a => a -> BS8.ByteString
   toBS = BS8.pack . show

instance ToByteString Int
instance ToByteString Bool
instance ToByteString Text where
   toBS = T.encodeUtf8

-- | Helper function for packing query items.
-- Original interface of setRequestQueryString can be messy.
-- Look into API.Telegram for usage example.
packQVal :: ToByteString a => a -> Maybe BS8.ByteString
packQVal a = Just $ toBS a

-- >>