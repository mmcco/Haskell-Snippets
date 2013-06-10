{-# LANGUAGE OverloadedStrings #-}
import Data.Aeson
import qualified System.Process as Process
import qualified Data.ByteString.Lazy.Char8 as BL
import Control.Applicative ((<$>), (<*>))
import Control.Monad (mzero)

data Block = Block {
    blockHash :: String,
    -- must make this a list of strings
    txs :: String,
    prevHash :: String
}

data Tx = Tx {
    txHash :: String
}

instance FromJSON Block where
    parseJSON (Object v) =
        Block <$>
        (v .: "hash") <*>
        (v .: "transactions") <*>
        (v .: "previousblockhash")
--    parseJSON _ = mzero

main = do
    chainHeight <- Process.readProcess "bitcoind" ["getblockcount"] []
    firstHash <- Process.readProcess "bitcoind" ["getblockhash", chainHeight] []
    blockInfo <- Process.readProcess "bitcoind" ["getblock", firstHash] []
    let jsonData = decode $ BL.pack blockInfo :: Maybe Block
    print "compiled"
