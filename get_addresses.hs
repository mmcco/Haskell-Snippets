-- this script takes an output file from the allBalances command of the blockparser program and returns a list of the addresses it contains

import qualified Data.List as List

getAddresses :: String -> [String]
getAddresses x = List.nub .
                 filter ((/=) "XXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXX") .
                 map (flip (!!) 2) .
                 filter (\x -> (>=) (length x) 3) .
                 map words .
                 drop 3 .
                 lines $ x

shortLines :: String -> [(Int, [String])]
shortLines x = filter (\x -> (<) (length (snd x)) 3) .
               (\xs -> zip [1..length xs] (map words xs)) .
               lines $ x

main = do
   transactions <- getContents
   mapM_ print $ getAddresses transactions
