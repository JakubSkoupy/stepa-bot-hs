-- allows "string literals" to be Text
{-# LANGUAGE OverloadedStrings #-}

module Main (main) where

import Control.Monad (void, when)
import Data.Maybe (fromMaybe)
import Data.Text (Text, isPrefixOf, pack, split, take, toLower, unpack, unwords)
import qualified Data.Text as T
import Discord
import qualified Discord.Requests as R
import Discord.Types
import LoadEnv (loadEnv)
import StringUtils as S
import System.Environment (lookupEnv)

main :: IO ()
main = do
  _ <- loadEnv
  token <- lookupEnv "DISCORD_BOT_TOKEN"
  let tokenStr = fromMaybe "No token found" token
  let dsOptions = def {discordToken = pack tokenStr, discordOnEvent = eventHandler}

  putStrLn "StepaBot starting"
  runDiscord dsOptions
  pure ()

eventHandler :: Event -> DiscordHandler ()
eventHandler event = case event of
  MessageCreate message ->
    let id = messageChannelId message
     in let (isCmd, args) = S.getCommand (messageContent message)
         in when (isCmd && not (fromBot message)) $ void $ handleCommand message args
  _ -> pure ()

handleCommand :: Message -> [Text] -> DiscordHandler ()
handleCommand m ("boxeri" : args) = boxeri m args
handleCommand m _ = sendUnknown m

boxeri :: Message -> [Text] -> DiscordHandler ()
boxeri msg args =
  let id = messageChannelId msg
   in void $ restCall (R.CreateMessage id (pack "Jak rikaji boxeri, " <> (T.unwords args)))

sendUnknown :: Message -> DiscordHandler ()
sendUnknown msg = void $ restCall (R.CreateMessage (messageChannelId msg) "Tak tenhle prikaz neznam!")
