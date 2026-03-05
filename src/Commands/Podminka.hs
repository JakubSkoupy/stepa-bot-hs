{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE TypeApplications #-}

module Commands.Podminka (podminka, PodminkaRecord) where

import Common
import Control.Exception (SomeException (SomeException))
import qualified Control.Exception as E
import Control.Monad ()
import Control.Monad.IO.Class
import Data.Aeson
import Data.Aeson.TH (deriveFromJSON)
import Data.Text hiding (show)
import qualified Data.Text as T hiding (show)
import Discord
import qualified Discord.Requests as R
import Discord.Types
import GHC.Generics
import Network.HTTP.Simple
import StringUtils
import System.IO (hPutStrLn, stderr)
import Utils.ApiUtils (apiGet, apiGetObj, apiGetObjPreprocess, unwrapSingleKey)

apiAddress = pack "https://api.iot.boulderama.cz/api/climate"

-- Top level command handler
podminka :: Message -> [Text] -> DiscordHandler ()
podminka msg [] = do
  response <- liftIO handlePodminkaRequest
  let responseMessage = case response of
        Nothing -> pack "Nejak se mi to nepodarilo zjistit"
        Just x -> formatApiResponse x
  sendToMessageChannel msg responseMessage
podminka msg args = sendToMessageChannel msg $ podminkaStr args

------------------------------------------------------------------------------------------
--- API HANDLING
------------------------------------------------------------------------------------------

data PodminkaRecord = PodminkaRecord {humidity :: Double, temperature :: Double} deriving (Show, Generic, FromJSON)

-- Provides a message based on the returned API object (PodminkaRecord)
formatApiResponse :: PodminkaRecord -> Text
formatApiResponse rec = pack $ "Teplota: " ++ (show $ temperature rec) ++ " Vlhkost: " ++ (show $ humidity rec)

-- Performs the API request and returns the API object on success
handlePodminkaRequest :: IO (Maybe PodminkaRecord)
handlePodminkaRequest =
  let preprocessor = (`unwrapSingleKey` 1)
   in apiGetObjPreprocess @PodminkaRecord apiAddress preprocessor

ioToMaybe :: IO a -> IO (Maybe a)
ioToMaybe action = (fmap Just action) `E.catch` exceptionHandler
  where
    exceptionHandler :: SomeException -> IO (Maybe a)
    exceptionHandler _ = pure Nothing

------------------------------------------------------------------------------------------
--- Trivial utils
------------------------------------------------------------------------------------------

-- Trivially returns a string message from the command arguments
podminkaStr :: [Text] -> Text
podminkaStr (x : _)
  | x == pack "pomoc" =
      pack "Proste napis !podminka, a ja ti reknu jak je na kotelne"
podminkaStr _ = pack "Ja nevim co s tim. Zkus pouzit !podminka pomoc"
