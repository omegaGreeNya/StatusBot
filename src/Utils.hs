-- | Different funtions to make life easier.
module Utils
   ( derivingDrop
   
   , (.<~)
   , (~>.)
   , (.<)
   , (>.)
   ) where

import Data.Aeson.TH (Options (..), defaultOptions)
import Data.Text (Text)
import qualified Data.Text as T

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