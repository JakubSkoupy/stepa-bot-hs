module Commands.Boxeri (boxeri) where

import Common
import Control.Monad
import Data.Text
import qualified Data.Text as T
import Discord
import qualified Discord.Requests as R
import Discord.Types
import StringUtils

boxeri :: Message -> [Text] -> DiscordHandler ()
boxeri msg [] = sendToMessageChannel msg $ pack "Jak rikaji boxeri, jsem vyplej!"
boxeri msg args =
  sendToMessageChannel msg $ pack "Jak rikaji boxeri, " <> (T.unwords args)
