-- should make a text file output that stores data about the information parsed (number of blocks, number of txs, etc.)
-- should clean up the function that gets insert values
-- need to decide how to associate outputs with inputs
-- create table statement for txs: CREATE TABLE txs (txHash TEXT UNIQUE NOT NULL, time INTEGER, coinbase TEXT, inputs TEXT, outputcalls TEXT, PRIMARY KEY(txHash));
-- create table statement for outputs: CREATE TABLE outputs (txHash TEXT, n INTEGER, time INTEGER, value REAL, addresses TEXT, inputs TEXT);
-- create table statement for outputs: CREATE TABLE outputs (txHASH TEXT NOT NULL, callNum INTEGER NOT NULL, addresses TEXT NOT NULL);
{-# LANGUAGE OverloadedStrings #-}
import Data.Aeson
import qualified System.Process as Process
import qualified Data.ByteString.Lazy.UTF8 as BL
import Control.Applicative ((<$>), (<*>))
import Control.Monad (mzero)
import qualified Database.HDBC as DB
import Database.HDBC.Sqlite3 (connectSqlite3, Connection)
import Data.ByteString.Lazy (intercalate)
import qualified Data.ByteString.Lazy as BB (concat)
import Data.Maybe (fromMaybe, maybeToList, catMaybes)

type BS = BL.ByteString

data Block = Block {
    blockHash :: BS,
    txs :: [BS],
    prevHash :: Maybe BS
} deriving (Show)

data Tx = Tx {
    txHash  :: BS,
    inputs  :: [Input],
    outputs :: [Output],
    time    :: Int
} deriving (Show)

-- the first tx in every block is a miner reward, and therefore has
-- a coinbase but no inputHash or outputCall
data Input = Input {
    coinbase   :: Maybe BS,
    inputHash  :: Maybe BS,
    outputCall :: Maybe Int
} deriving (Show)

data Output = Output {
    value :: Double,
    callNum :: Int,
    addresses :: [BS]
} deriving (Show)


instance FromJSON Block where
    parseJSON (Object v) =
        Block <$>
        (v .: "hash") <*>
        (v .: "tx") <*>
        (v .:? "previousblockhash")
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
        (v .: "n") <*>
        (v .: "scriptPubKey" >>= (.: "addresses"))
    parseJSON _ = mzero

-- helper function, concisely converts any showable type into a ByteString
byteString :: Show a => a -> BS
byteString = BL.fromString . show

-- gets the block associated with the supplied hash using bitcoind
getBlock :: BS -> IO (Maybe Block)
getBlock hash = do
                   let hashString = BL.toString hash
                   decode . BL.fromString <$> Process.readProcess "bitcoind" ["getblock", hashString] []

getTx :: BS -> IO (Maybe Tx)
getTx hash = do  
                txData <- Process.readProcess "bitcoind" ["getrawtransaction", BL.toString hash, "1"] []
                let maybeTx = decode . BL.fromString $ txData
                return maybeTx

txLoop :: Connection -> BS -> IO ()
txLoop conn hash = do
                       block <- getBlock hash
                       let txHashes = filter ((/=) "4a5e1e4baab89f3a32518a88c31bc87f618f76673e2cc77ab2127b7afdeda33b") . maybe [] txs $ block
                       blockTxs <- sequence . map getTx $ txHashes
                       let insertVals = map getInsertVals . catMaybes $ blockTxs
                           previousHash = maybe Nothing prevHash block
                       txInsert <- DB.prepare conn "INSERT INTO outputs VALUES (?, ?, ?, ?, ?, ?);"
                       DB.executeMany txInsert . map (map (DB.toSql . BL.toString)) . concat $ insertVals
                       maybe (return ()) (txLoop conn) previousHash

-- this is the logic used if each output is given its own row
getInsertVals :: Tx -> [[BS]]
getInsertVals tx = map (\x -> [hash, n x, txTime, txValue x, txAddresses x, txInputs]) . outputs $ tx
    where hash = txHash tx
          n = byteString . callNum
          txTime = byteString . time $ tx
          txValue = byteString . value
          txAddresses = intercalate "|" . addresses
          txInputs = intercalate "|" . map (\x -> BB.concat [fromMaybe "" $ inputHash x, " ", maybe "" byteString (outputCall x)]) . inputs $ tx

main = do
    chainHeight <- Process.readProcess "bitcoind" ["getblockcount"] []
    -- using low blockheight to make testing faster
    firstHash <- Process.readProcess "bitcoind" ["getblockhash", chainHeight] []
    conn <- connectSqlite3 "txs.db"
    txLoop conn (BL.fromString firstHash)
    DB.commit conn
    DB.disconnect conn
