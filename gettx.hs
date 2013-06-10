-- remember to filter blocks based on version
{-# LANGUAGE OverloadedStrings #-}
import Data.Aeson
import qualified System.Process as Process
import qualified Data.ByteString.Lazy.Char8 as BL
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

getBlocks :: IO BL.ByteString -> IO [Maybe Block]
getBlocks hash = (:) <$> block <*> getBlocks (lastHash block)
    where block = decode . BL.pack <$> Process.readProcess "bitcoind" ["getblock", BL.unpack hash] []
--          lastHash = maybe "" prevHash

lastHash :: IO (Maybe Block) -> IO BL.ByteString
lastHash = maybe "" prevHash

main = do
    chainHeight <- Process.readProcess "bitcoind" ["getblockcount"] []
    firstHash <- Process.readProcess "bitcoind" ["getblockhash", chainHeight] []
    print "compiled"
