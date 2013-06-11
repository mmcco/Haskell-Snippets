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

getTxs :: [Block] -> [BL.ByteString]
getTxs blocks = foldl1 (++) . map txs $ blocks

main = do
    chainHeight <- Process.readProcess "bitcoind" ["getblockcount"] []
    firstHash <- Process.readProcess "bitcoind" ["getblockhash", "1000"] []
    -- using low blockheight to make testing faster
    firstBlock <- getBlock . BL.fromString $ firstHash
    blocks <- blockLoop firstBlock
    print blocks
