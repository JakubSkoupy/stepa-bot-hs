module Commands.Podminka (podminka) where

import Common
import Control.Monad
import Control.Monad.IO.Class
import Data.Text
import qualified Data.Text as T
import Discord
import qualified Discord.Requests as R
import Discord.Types
import StringUtils

podminka :: Message -> [Text] -> DiscordHandler ()
podminka msg [] = liftIO handlePodminkaRequest >>= sendToMessageChannel msg
podminka msg args = sendToMessageChannel msg $ podminkaStr args

handlePodminkaRequest :: IO Text
handlePodminkaRequest = pure $ pack "No, slibuje to vedlejsi efekt, ale zadny nevidim"

podminkaStr :: [Text] -> Text
podminkaStr (x : _) | x == pack "pomoc" = pack "Proste napis !podminka, a ja ti reknu jak je na kotelne"
podminkaStr _ = pack "Ja nevim co s tim. Zkus pouzit !podminka pomoc"
