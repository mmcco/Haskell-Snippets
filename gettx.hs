-- the last block isn't being parsed
-- should clean up the function that gets insert values
-- what about the amount? need to review SQL table logic
-- create table statement for txs: CREATE TABLE txs (txHash TEXT UNIQUE NOT NULL, time INTEGER, coinbase TEXT, inputs TEXT, outputcalls TEXT, PRIMARY KEY(txHash));
-- create table statement for outputs: CREATE TABLE outputs (txHash TEXT, n INTEGER, time INTEGER, value REAL, addresses TEXT);
-- create table statement for outputs: CREATE TABLE outputs (txHASH TEXT NOT NULL, callNum INTEGER NOT NULL, addresses TEXT NOT NULL);
{-# LANGUAGE OverloadedStrings #-}
import Data.Aeson
import qualified System.Process as Process
import qualified Data.ByteString.Lazy.UTF8 as BL
import Control.Applicative ((<$>), (<*>))
import Control.Monad (mzero)
import qualified Database.HDBC as DB
import Database.HDBC.Sqlite3 (connectSqlite3)
import Data.ByteString.Lazy (intercalate)
import Data.Maybe (fromMaybe)

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

-- generates a list of all blocks before (and including) the supplied block
blockLoop :: Maybe Block -> IO [Block]
blockLoop Nothing = return []
blockLoop (Just block) = if previousHash == "" then return [block] else 
                            do
                                lastBlock <- getBlock $ previousHash
                                rest <- blockLoop lastBlock
                                return (block : rest)
    where previousHash = fromMaybe "" (prevHash block)

-- gets the block associated with the supplied hash using bitcoind
getBlock :: BS -> IO (Maybe Block)
getBlock hash = do
                    let hashString = BL.toString hash
                    decode . BL.fromString <$> Process.readProcess "bitcoind" ["getblock", hashString] []

getTxs :: [Block] -> IO [Tx]
getTxs = txLoop . foldl1 (++) . map txs -- generate a list of all the blocks' txs and apply txLoop to it
    where txLoop [] = return []
          txLoop (first:others) = do
                                      txData <- Process.readProcess "bitcoind" ["getrawtransaction", BL.toString first, "1"] []
                                      let maybeTx = decode . BL.fromString $ txData
                                      let tx = maybe [] (\x -> [x]) maybeTx
                                      rest <- txLoop others
                                      return (tx ++ rest)

-- helper function, concisely converts any showable type into a ByteString
byteString :: Show a => a -> BS
byteString = BL.fromString . show

-- takes a tx and returns a list of values for an insert
--getInsertVals :: Tx -> [BS] --(BS, Int, BS, BS, BS)
--getInsertVals tx = [hash, txTime, txCoinbase, inputHashes, outputCalls]
--    where hash        = txHash tx
--          txTime      = byteString . time $ tx
--          txCoinbase  = maybe "" id . coinbase . head . inputs $ tx
--          inputHashes = intercalate "|" . map (maybe "" id) . map inputHash . inputs $ tx
--          outputCalls = intercalate "|" . map (maybe "" byteString) . map outputCall . inputs $ tx

-- this is the logic used if each output is given its own row
getInsertVals :: Tx -> [[BS]]
getInsertVals tx = map (\x -> hash : (byteString . callNum $ x) : txTime : (byteString . value $ x) : (intercalate "|" $ addresses x) : []) . outputs $ tx
    where hash = txHash tx
          txTime = byteString . time $ tx

main = do
    chainHeight <- Process.readProcess "bitcoind" ["getblockcount"] []
    -- using low blockheight to make testing faster
    firstHash <- Process.readProcess "bitcoind" ["getblockhash", "0"] []
    firstBlock <- getBlock . BL.fromString $ firstHash
    blocks <- blockLoop firstBlock
    print $ "length of blocks: " ++ (show . length $ blocks)
    --print ("blocks length: " ++ (show . length $ blocks))
    --txs <- getTxs blocks
    --print . getInsertVals . last $ txs
    --map print . map (intercalate "|") . inputs $ txs
    --print . take 50 $ txs
    --print $ "length txs: " ++ (show . length $ txs)
    -- generate a two-dimensional list of values to supply to the database insert
    --conn <- connectSqlite3 "txs.db"
    --txInsert <- DB.prepare conn "INSERT INTO outputs VALUES (?, ?, ?, ?, ?);"
    --print $ map (map DB.toSql . map BL.toString . getInsertVals) txs
    --let insertVals = concat . map getInsertVals $ txs
    --DB.executeMany txInsert $ map (map (DB.toSql . BL.toString)) insertVals
    --DB.commit conn
    --DB.disconnect conn
