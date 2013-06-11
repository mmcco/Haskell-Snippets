-- remember to filter blocks based on version
{-# LANGUAGE OverloadedStrings #-}
import Data.Aeson
import qualified System.Process as Process
import qualified Data.ByteString.Lazy.UTF8 as BL
import Control.Applicative ((<$>), (<*>))
import Control.Monad (mzero)

data Block = Block {
    blockHash :: BL.ByteString,
    txs :: [BL.ByteString],
    prevHash :: BL.ByteString
} deriving (Show)

instance FromJSON Block where
    parseJSON (Object v) =
        Block <$>
        (v .: "hash") <*>
        (v .: "tx") <*>
        (v .: "previousblockhash")
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

getTxs :: [Block] -> IO [String]
getTxs [] = return []
getTxs blocks = do
                    txData  <- txCommand . map BL.toString . txs . head $ blocks
                    rest <- getTxs $ tail blocks
                    return (txData ++ rest)
    where txCommand [] = return []
          txCommand txs = do
                              response <- Process.readProcess "bitcoind" ["getrawtransaction", head txs, "1"] []
                              rest <- txCommand $ tail txs
                              return (response : rest)

myfunc :: [Block] -> [String]
myfunc blocks = map BL.toString . txs . head $ blocks

main = do
    chainHeight <- Process.readProcess "bitcoind" ["getblockcount"] []
    -- using low blockheight to make testing faster
    firstHash <- Process.readProcess "bitcoind" ["getblockhash", "50"] []
    firstBlock <- getBlock . BL.fromString $ firstHash
    blocks <- blockLoop firstBlock
    txs <- getTxs blocks
    print txs
