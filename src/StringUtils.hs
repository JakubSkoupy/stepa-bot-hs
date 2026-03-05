{-# LANGUAGE OverloadedStrings #-}

module StringUtils where

import Data.Text
import qualified Data.Text as T
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
