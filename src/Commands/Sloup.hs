module Commands.Sloup (sloup) where

import Common
import Control.Applicative (Const (Const))
import Control.Arrow
import Control.Arrow.ArrowTree
import Control.Exception (SomeException (SomeException))
import qualified Control.Exception as E
import Control.Monad (join)
import Control.Monad.IO.Class
import Data.Aeson
import Data.Aeson.TH (deriveFromJSON)
import Data.Function ((&))
import Data.Maybe (fromMaybe, mapMaybe)
import Data.Text (Text, pack)
import qualified Data.Text as T hiding (show)
import Discord
import Discord.Internal.Rest.HTTP (JsonRequest (Get))
import qualified Discord.Requests as R
import Discord.Types
import GHC.Base (Alternative ((<|>)))
import qualified GHC.Base as B
import GHC.Generics
import Network.HTTP.Simple
import StringUtils
import System.IO (hPutStrLn, stderr)
import Text.XML.HXT.Core
import Text.XML.HXT.Curl
import Text.XML.HXT.Parser.XmlTokenParser (qName)
import Text.XML.HXT.XPath (getXPathTrees)
import Text.XML.HXT.XPath.Arrows
import Text.XML.HXT.XPath.XPathDataTypes
import Text.XML.HXT.XPath.XPathParser
import Utils.ApiUtils (apiGet, apiGetObj, apiGetObjPreprocess, unwrapSingleKey)

{- ORMOLU_DISABLE -}
apiAddress = "https://hydro.chmi.cz/hppsoldv/hpps_prfdata.php?seq=38908612"
xpath_l_table = "/html/body/div[1]/div[3]/div/table[2]/tr[3]/td/div/table"
-- xpath_tab =     "/html/body/div[1]/div/div/table[2]/tbody/tr[4]/td/div/table/tbody"
xpath_tab =        "/html/body/div[1]/div/div/table[2]/tr[4]/td/div/table"
xpath_l_table_extended = "/html/body/div[1]/div/div/table[2]/tr[4]/td/div/table"
{- ORMOLU_ENABLE -}

data SloupTable = SloupTable {rows :: [Text]}

-- Top level command handler
sloup :: Message -> [Text] -> DiscordHandler ()
sloup msg [] = do
  response <- liftIO handleSloupRequest
  sendToMessageChannel msg $ response
sloup msg args = sendToMessageChannel msg $ sloupStr args

handleSloupRequest :: IO Text
handleSloupRequest = do
  tableM <- ioToMaybe $ scrapeSloupTable apiAddress xpath_tab
  pure $ fromMaybe (pack "Nic jsem nezjistil") $ fmap formatSloupTable (join tableM)

headMay :: [a] -> Maybe a
headMay (x : xx) = Just x
headMay _ = Nothing

ioToMaybe :: IO a -> IO (Maybe a)
ioToMaybe action = (fmap Just action) `E.catch` exceptionHandler
  where
    exceptionHandler :: SomeException -> IO (Maybe a)
    exceptionHandler _ = pure Nothing

------------------------------------------------------------------------------------------
--- Scraping utils
------------------------------------------------------------------------------------------
curlXpath :: String -> String -> IO [XmlTree]
curlXpath url xpath = do
  let doc = readDocument [withParseHTML yes, withWarnings no, withCurl []] url
  nodes <- runX $ doc >>> getXPathTrees xpath
  pure nodes

data SloupTableRow = SloupTableRow {date :: Text, levelCm :: Text, bandwidth :: Text}
  deriving (Show)

-- Scrapes the table from the given URL and XPath expression
-- The table expect a stricly given format.
scrapeSloupTable :: String -> String -> IO (Maybe [SloupTableRow])
scrapeSloupTable url xpathExpr = do
  element <- curlXpath url xpathExpr
  pure $ headMay element >>= processSloupTable

-- Parses a table format from a given XML tree, returning a list of SloupTableRow on success
processSloupTable :: XmlTree -> Maybe [SloupTableRow]
processSloupTable tree@(NTree (XTag _ _) children) =
  let rowTexts = map processSloupTableRow children
      filteredTexts = take 15 $ mapMaybe id rowTexts
   in if null filteredTexts
        then Nothing
        else Just filteredTexts

-- Parses a table row from a given XML tree, returning a SloupTableRow on success
processSloupTableRow :: XmlTree -> Maybe SloupTableRow
processSloupTableRow tree =
  case tree of
    NTree (XTag qname _) children
      | qualifiedName qname == "tr" ->
          let cellTexts = mapMaybe extractCellText children
           in constructSloupTableRow cellTexts
    _ -> Nothing

-- Constructs a SloupTableRow from a list of cell texts, expecting at least 3 cells (date, levelCm, bandwidth)
constructSloupTableRow :: [Text] -> Maybe SloupTableRow
constructSloupTableRow (d : l : b : _) = Just $ SloupTableRow {date = d, levelCm = l, bandwidth = b}
constructSloupTableRow row = Just $ SloupTableRow {date = pack "-", levelCm = pack "-", bandwidth = pack "-"}

deconstructSloupTableRow :: SloupTableRow -> [Text]
deconstructSloupTableRow row = [date row, levelCm row, bandwidth row]

-- Extracts text from a table cell (td) in a given XML tree, returning the text if the node is a td element
extractCellText :: XmlTree -> Maybe Text
extractCellText (NTree (XTag qname _) children)
  | qualifiedName qname == "td" =
      case mapMaybe extractTextNode children of
        [] -> Nothing
        xs -> Just $ T.concat xs
extractCellText _ = Nothing

-- Extracts text from a given XML tree node.
extractTextNode :: XmlTree -> Maybe Text
extractTextNode (NTree (XText str) _) = Just (T.pack str)
extractTextNode _ = Nothing

------------------------------------------------------------------------------------------
--- Formatting utils
------------------------------------------------------------------------------------------
formatSloupRow :: SloupTableRow -> Text
formatSloupRow row = pack "| " <> (T.intercalate (pack " | ") $ deconstructSloupTableRow row) <> pack " |"

formatSloupTable :: [SloupTableRow] -> Text
formatSloupTable table =
  pack "------------------------\n"
    <> (T.intercalate (pack "\n") $ map formatSloupRow $ drop 1 table)

-- Trivially returns a string message from the command arguments
sloupStr :: [Text] -> Text
sloupStr (x : _)
  | x == pack "pomoc" =
      pack "Proste napis !sloup, a ja ti reknu jak vypada hlaseni"
sloupStr _ = pack "Ja nevim co s tim. Zkus pouzit !sloup pomoc"
