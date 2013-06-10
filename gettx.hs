-- I am first writing a script that will generate a list of all of the blocks in the current chain
import qualified System.Process as Process
import qualified Text.JSON as JSON

data Block = Block {
    hash :: String
    lastHash :: String
}

getBlock :: String -> Maybe Block
getBlock = maybe (error "Shell call to bitcoind failed") (\(Just a) -> JSON.decode a)
    where sysResponse = Process.readProcess "bitcoind" ["getblockhash", hash]

txLoop :: Maybe String -> [String]
txLoop (Ok ) = block : (txLoop lastHash)
txLoop (Error a) = error a
    where lastHash = 
