-- this script takes an output file from the allBalances command of the blockparser program and returns a list of the addresses it contains

import qualified Data.List as List

getAddresses :: String -> [String]
getAddresses x = List.nub .
                 filter ((/=) "XXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXX") . 
                 map (flip (!!) 2) . 
                 map words . 
                 drop 3 .
                 lines $ x

main = do
   transactions <- getContents
   mapM_ print $ getAddresses transactions
