{-# LANGUAGE OverloadedStrings #-}

module StringUtils where

import Control.Monad
import Data.Text
import qualified Data.Text as T
import Discord
import qualified Discord.Requests as R
import Discord.Types

fromBot :: Message -> Bool
fromBot = userIsBot . messageAuthor

isCommand :: Text -> Bool
isCommand = ("!" `isPrefixOf`)

getCommandArgs :: Text -> [Text]
getCommandArgs = split (== ' ') . T.drop 1

getCommand :: Text -> (Bool, [Text])
getCommand message
  | isCommand message = (True, getCommandArgs message)
  | otherwise = (False, [])

sendToChannel :: ChannelId -> Text -> DiscordHandler ()
sendToChannel id msg = void $ restCall (R.CreateMessage id msg)

sendToMessageChannel :: Message -> Text -> DiscordHandler ()
sendToMessageChannel msg = sendToChannel $ messageChannelId msg
