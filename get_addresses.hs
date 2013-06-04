-- this script takes an output file from the allBalances command of the blockparser program and returns a list of the addresses it contains

import qualified Data.List as List

getAddresses :: String -> [String]
getAddresses = List.nub .
                 filter ((/=) "XXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXX") .
                 map (flip (!!) 2) .
                 filter (\x -> (>=) (length x) 3) .
                 map words .
                 drop 3 .
                 lines

shortLines :: String -> [(Int, String)]
shortLines = filter (\x -> (<) (length $ words $ snd x) 3) .
               zip [1..] .
               lines

main = do
   transactions <- getContents
   mapM_ print $ getAddresses transactions
