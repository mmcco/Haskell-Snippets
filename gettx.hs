{-# LANGUAGE OverloadedStrings #-}

import qualified System.Process as Process
import Control.Monad
import Data.Aeson ((.:), (.:?), decode, FromJSON(..), Value(..))
import Control.Applicative ((<$>), (<*>))
import qualified Data.ByteString.Lazy as B
import qualified Data.ByteString.Lazy.Char8 as Char8

-- helper function, removes n items from the end of the supplied list
chop :: Int -> [a] -> [a]
chop n xs = zipWith const xs (drop n xs)

data Block = Block {
    tx :: [B.ByteString],
    previousblockhash :: B.ByteString
} deriving (Show)

data Tx = Tx {
    id :: B.ByteString
} deriving (Show)

instance FromJSON Block where
    parseJSON (Object v) =
        Block <$>
        (v .: "tx") <*>
        (v .: "previousblockhash")
{-
main = do
    blockCount <- Process.readProcess "bitcoind" ["getblockcount"] []
    firstBlock <- Process.readProcess "bitcoind" ["getblockhash", blockCount] []
    --let getBlocks lst:blocks = (getBlocks $ previousblockhash JSON lst):lst:blocks
    useJSON (Just a) <- print . init . previousblockhash a
    useJSON Nothing <- print "error parsing block"
    getJSON block <- useJSON $ decode (Process.readProcess "bitcoind" ["getblock", block])
    print firstBlock
    getJSON firstBlock
-}
main = do
    blockCount <- Process.readProcess "bitcoind" ["getblockcount"] []
    firstBlock <- init $ Process.readProcess "bitcoind" ["getblockhash", blockCount] []

