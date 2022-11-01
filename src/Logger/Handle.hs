-- | Inner module. Defines Logger handle and log message levels.
module Logger.Handle
    ( Handle(..)
    , LogLevel(..)
    ) where

import Data.Text (Text)

-- | Logger handle.
data Handle m = Handle
   { hLogMessage :: LogLevel -> Text -> m ()
   -- ^ Actual implementation abstracted
   -- behind this function.
   }

-- | Logs can be presented with different levels
-- hLogMessage may be implimented to suppres some levels
-- or to markdown messages diferenly, or even to rise an exception.
data LogLevel
   = DEBUG
   | INFO
   | WARN
   | ERROR
   deriving (Show, Eq, Ord)