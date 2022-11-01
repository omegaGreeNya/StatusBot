-- | Some app constants defined here.
-- Default app confing defined in Initialization module, however.
-- It might be good idea to even separate default app cfg to it's own module.
module Constants
   ( configPath
   ) where

configPath :: FilePath
configPath = "config.json"