{-# LANGUAGE OverloadedStrings #-}

import Data.Aeson ((.:), (.:?), decode, FromJSON(..), Value(..))
import qualified Data.ByteString.Lazy.Char8 as BL
import Control.Applicative ((<$>), (<*>))
import Database.HDBC.Sqlite3 (connectSqlite3)
import qualified Database.HDBC as DB

data Trade = Trade {
   date :: Int,
   price :: String,
   amount :: String,
   price_int :: String,
   amount_int :: String,
   tid :: String,
   price_currency :: String,
   item :: String,
   trade_type :: String,
   primary :: String,
   properties :: String
} deriving (Show)

-- logic for the Aeson parser of the initial data
instance FromJSON Data where
   parseJSON (Object v) = 
      Data <$>
      (v .: "result")  <*>
      (v .: "return")

-- logic for the Aeson parser of the trade data
instance FromJSON Trade where
   parseJSON (Object v) = 
      Trade <$>
      (v .: "date")             <*>
      (v .: "price")            <*>
      (v .: "amount")           <*>
      (v .: "price_int")        <*>
      (v .: "amount_int")       <*>
      (v .: "tid")              <*>
      (v .: "price_currency")   <*>
      (v .: "item")             <*>
      (v .: "trade_type")       <*>
      (v .: "primary")          <*>
      (v .: "properties")
   
-- stores the initial JSON data received
data Data = Data {
   result :: String,   -- "result" tells us whether the JSON query was successful
   trades :: [Trade]   -- "trades" contains a list of each trade's information, which is then itself parsed
} deriving (Show)

-- takes a list of trades and returns a list of lists that can be used as parameters in a SQL statement
process :: Maybe Data -> [[String]]
process Nothing = []
process (Just x) = map (\(Trade a b c d e f g h i j k) -> [(show a), d, e, f, g, h, i, j, k]) myTrades -- skips price and amount because they are large and redundant
   where myTrades = trades x

main = do
   contents <- BL.getContents
   let myData = decode contents :: Maybe Data
       myTrades = process myData
   conn <- connectSqlite3 "trades.db"
   DB.disconnect conn
