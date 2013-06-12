-- a problem with parsing inputs and outputs is preventing transactions from being parsed
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

-- the first tx in every block is a miner reward, and therefore has
-- a coinbase but no inputHash or outputCall
data Input = Input {
    coinbase   :: Maybe BL.ByteString,
    inputHash  :: Maybe BL.ByteString,
    outputCall :: Maybe Int
} deriving (Show)

data Output = Output {
    value :: Double,
    callNum :: Int--,
    --addresses :: BL.ByteString
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
        (v .:? "coinbase") <*>
        (v .:? "txid") <*>
        (v .:? "vout")
    parseJSON _ = mzero

instance FromJSON Output where
    parseJSON (Object v) =
        Output <$>
        (v .: "value") <*>
        (v .: "n") -- <*>
       -- (v .: "scriptPubKey") -- >>= (.: "addresses"))
    parseJSON _ = mzero

-- generates a list of all blocks before (and including) the supplied block
blockLoop :: Maybe Block -> IO [Block]
blockLoop Nothing = return []
blockLoop (Just block) = do
    lastBlock <- getBlock $ prevHash block
    rest <- blockLoop lastBlock
    return (block : rest)

-- gets the block associated with the supplied hash using bitcoind
getBlock :: BL.ByteString -> IO (Maybe Block)
getBlock hash = do
                    let hashString = BL.toString hash
                    decode . BL.fromString <$> Process.readProcess "bitcoind" ["getblock", hashString] []

getTxs :: [Block] -> IO [Tx]
getTxs = txLoop . foldl1 (++) . map txs -- generate a list of all the blocks' txs and apply txLoop to it
    where txLoop [] = return []
          txLoop (first:others) = do
                                      txData <- Process.readProcess "bitcoind" ["getrawtransaction", BL.toString first, "1"] []
                                      print $ txData
                                      let maybeTx = decode . BL.fromString $ txData
                                      let tx = maybe [] (\x -> [x]) maybeTx
                                      rest <- txLoop others
                                      return (tx ++ rest)

main = do
    chainHeight <- Process.readProcess "bitcoind" ["getblockcount"] []
    -- using low blockheight to make testing faster
    firstHash <- Process.readProcess "bitcoind" ["getblockhash", "5"] []
    firstBlock <- getBlock . BL.fromString $ firstHash
    blocks <- blockLoop firstBlock
    print ("blocks length: " ++ (show . length $ blocks))
    txs <- getTxs blocks
    print . take 50 $ txs
    print $ "length txs: " ++ (show . length $ txs)
    -- generate a two-dimensional list of values to supply to the database insert
    --let values = map (map DB.toSql . (\x -> [txHash x, BL.fromString . show . time $ x])) txs
    --conn <- connectSqlite3 "txs.db"
    --txInsert <- DB.prepare conn "INSERT INTO txs VALUES (?, ?);"
    --DB.executeMany txInsert values
    --DB.commit conn
    --DB.disconnect conn
