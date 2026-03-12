{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeApplications #-}

module Utils.ApiUtils where

import Control.Exception
import qualified Control.Exception as E
import Data.Aeson
import qualified Data.ByteString.Lazy.Char8 as L8
import qualified Data.Map.Strict as M
import Data.Maybe (listToMaybe)
import Data.Text
import qualified Data.Text as T
import qualified Data.Text.Encoding as TE
import qualified Data.Text.IO as TIO
import qualified Data.Text.Lazy as TL
import qualified Data.Text.Lazy.Encoding as TL8
import Network.HTTP.Simple

------------------------------------------------------------------------------------------
--- API CLIENT ACTIONS
------------------------------------------------------------------------------------------
apiGet :: Text -> IO Text
apiGet url = do
  request <- parseRequest $ unpack url
  response <- httpLBS request
  return $ pack $ L8.unpack $ getResponseBody response

apiGetObj :: (FromJSON a) => Text -> IO (Maybe a)
apiGetObj url = do
  request <- parseRequest $ unpack url
  response <- httpLBS request
  return $ decode $ getResponseBody response

apiGetObjPreprocess :: (FromJSON a) => Text -> (L8.ByteString -> L8.ByteString) -> IO (Maybe a)
apiGetObjPreprocess url preprocessor = do
  request <- parseRequest $ unpack url
  response <- httpLBS request
  let cleaned = preprocessor (getResponseBody response)
  decodeWithLog cleaned

ioToMaybe :: IO a -> IO (Maybe a)
ioToMaybe action = (fmap Just action) `E.catch` exceptionHandler
  where
    exceptionHandler :: SomeException -> IO (Maybe a)
    exceptionHandler _ = pure Nothing

------------------------------------------------------------------------------------------
--- JSON UTILS
------------------------------------------------------------------------------------------

-- | Unwrap a single-key JSON object after removing N outer braces
unwrapSingleKey :: L8.ByteString -> Int -> L8.ByteString
unwrapSingleKey bs n =
  let -- convert lazy ByteString to lazy Text
      txt = TL8.decodeUtf8 bs
      -- remove outer braces (n layers)
      stripped = stripOuterTL txt n
      -- extract inner JSON value ignoring the outer key
      inner = extractInner stripped
      -- convert back to lazy ByteString
      result = TL8.encodeUtf8 inner
   in result

-- | Extract inner JSON value by ignoring the first key
extractInner :: TL.Text -> TL.Text
extractInner t =
  case TL.splitOn ":" t of
    (_key : rest) -> TL.intercalate ":" rest
    _ -> t

-- | Decode a ByteString to a value, logging any parse errors along with the original JSON
decodeWithLog :: (FromJSON a) => L8.ByteString -> IO (Maybe a)
decodeWithLog bs =
  case eitherDecode bs of
    Left err -> do
      let rawText = TE.decodeUtf8 (L8.toStrict bs) -- convert ByteString to Text
      TIO.putStrLn $ "JSON parse error: " <> T.pack err
      TIO.putStrLn $ "Original JSON: " <> rawText
      pure Nothing
    Right val -> pure (Just val)

------------------------------------------------------------------------------------------
--- STRING UTILS (TODO make generic and move to string utils)
------------------------------------------------------------------------------------------

-- | Remove outer braces N times from a lazy ByteString
stripOuterL8 :: L8.ByteString -> Int -> L8.ByteString
stripOuterL8 bs n =
  TL8.encodeUtf8 $ stripOuterTL (TL8.decodeUtf8 bs) n

-- | Remove outer braces N times from lazy Text
stripOuterTL :: TL.Text -> Int -> TL.Text
stripOuterTL t 0 = t
stripOuterTL t n = stripOuterTL (stripOuterBracesTL t) (n - 1)

-- | Remove a single layer of surrounding braces from lazy Text
stripOuterBracesTL :: TL.Text -> TL.Text
stripOuterBracesTL t = dropAroundSingle (TL.strip t) (\c -> c == '{' || c == '}')

-- | Drop at most one character from the start and end if it satisfies the predicate
dropAroundSingle :: TL.Text -> (Char -> Bool) -> TL.Text
dropAroundSingle t p =
  let t' = if not (TL.null t) && p (TL.head t) then TL.tail t else t
      t'' = if not (TL.null t') && p (TL.last t') then TL.init t' else t'
   in t''
