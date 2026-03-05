module Common where

import Control.Monad (void, when)
import Data.Maybe (fromMaybe)
import Data.Text (Text, isPrefixOf, pack, split, take, toLower, unpack, unwords)
import Discord
import Discord.Types
import LoadEnv (loadEnv)
