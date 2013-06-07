import qualified System.Process as Process
import Control.Monad
import Data.Aeson ((.:), (.:?), decode, FromJSON(..), Value(..))

-- helper function, removes n items from the end of the supplied list
chop :: Int -> [a] -> [a]
chop n xs = zipWith const xs (drop n xs)

data Block = Block {
    tx :: [String],
    previousblockhash :: String
} deriving (Show)

data Tx = Tx {
    id :: String
} deriving (Show)

instance FromJSON Data where
    parseJSON (Object v) =
        Data <$>
        (v .: "tx") <*>
        (v .: "previousblockhash")

main = do
    blockCount <- Process.readProcess "bitcoind" ["getblockcount"] []
    firstBlock <- Process.readProcess "bitcoind" ["getblockhash", blockCount] []
    let getBlocks lst:blocks = (getBlocks $ previousblockhash lstJSON):lst:blocks
        where lstJSON = decode blocks :: Maybe Block
    print . init $ firstHash
