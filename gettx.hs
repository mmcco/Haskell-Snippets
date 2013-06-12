-- remember to filter blocks based on version
{-# LANGUAGE OverloadedStrings #-}
import Data.Aeson
import qualified System.Process as Process
import qualified Data.ByteString.Lazy.UTF8 as BL
import Control.Applicative ((<$>), (<*>))
import Control.Monad (mzero)
import qualified Database.HDBC as DB
import Database.HDBC.Sqlite3 (connectSqlite3)

data Block = Block {
    blockHash :: BL.ByteString,
    txs :: [BL.ByteString],
    prevHash :: BL.ByteString
} deriving (Show)

data Tx = Tx {
    txHash  :: BL.ByteString,
    inputs  :: [Input],
    outputs :: [Output],
    time    :: Int
} deriving (Show)

data Input = Input {
    inputHash  :: BL.ByteString,
    outputCall :: Int
} deriving (Show)


data Output = Output {
    callNum :: Int,
    value   :: Double,
    addresses :: [BL.ByteString]
} deriving (Show)


instance FromJSON Block where
    parseJSON (Object v) =
        Block <$>
        (v .: "hash") <*>
        (v .: "tx") <*>
        (v .: "previousblockhash")
    parseJSON _ = mzero

instance FromJSON Tx where
    parseJSON (Object v) =
        Tx <$>
        (v .: "txid") <*>
        (v .: "vin") <*>
        (v .: "vout") <*>
        (v .: "time")
    parseJSON _ = mzero

instance FromJSON Input where
    parseJSON (Object v) =
        Input <$>
        (v .: "txid") <*>
        (v .: "vout")
    parseJSON _ = mzero

instance FromJSON Output where
    parseJSON (Object v) =
        Output <$>
        (v .: "txid") <*>
        (v .: "value") <*>
        (v .: "scriptPubKey" >>= (.: "addresses"))
    parseJSON _ = mzero

blockLoop :: Maybe Block -> IO [Block]
blockLoop Nothing = return []
blockLoop (Just block) = do
    lastBlock <- getBlock $ prevHash block
    rest <- blockLoop lastBlock
    return (block : rest)

getBlock :: BL.ByteString -> IO (Maybe Block)
getBlock hash = do
                    let hashString = BL.toString hash
                    decode . BL.fromString <$> Process.readProcess "bitcoind" ["getblock", hashString] []
{-
getTxs :: [Block] -> IO [BL.ByteString]
getTxs [] = return []
getTxs blocks = do
                    txData  <- txCommand . map BL.toString . txs . head $ blocks
                    rest <- getTxs $ tail blocks
                    return (txData ++ rest)
    where txCommand [] = return []
          txCommand txs = do
                              response <- BL.fromString <$> Process.readProcess "bitcoind" ["getrawtransaction", head txs, "1"] []
                              rest <- txCommand $ tail txs
                              return (response : rest)
-}
getTxs :: [Block] -> IO [Tx]
getTxs = txLoop . foldl1 (++) . map txs
    where txLoop [] = return []
          txLoop (first:others) = do
                                      txData <- Process.readProcess "bitcoind" ["getrawtransaction", BL.toString first, "1"] []
                                      let maybeTx = decode . BL.fromString $ txData
                                      let tx = maybe [] (\x -> (:) x []) maybeTx
                                      rest <- txLoop others
                                      return (tx ++ rest)

main = do
    chainHeight <- Process.readProcess "bitcoind" ["getblockcount"] []
    -- using low blockheight to make testing faster
    firstHash <- Process.readProcess "bitcoind" ["getblockhash", "50"] []
    firstBlock <- getBlock . BL.fromString $ firstHash
    blocks <- blockLoop firstBlock
    txs <- getTxs blocks
    let values = map (map DB.toSql) . map (\x -> [(txHash x), (BL.fromString . show . time $ x)]) $ txs
    conn <- connectSqlite3 "txs.db"
    txInsert <- DB.prepare conn "INSERT INTO txs VALUES ();"
    DB.executeMany txInsert values
    DB.commit conn
    DB.disconnect conn
