-- This script parses raw data from the genotyping service 23andMe
import Data.List as List
import Control.Monad as Monad

main = do
   contents <- getContents
   let errthang = populate contents
       snps = map rsid errthang
   mapM_ putStrLn snps

data SNP = SNP { rsid :: String
           , chromosome :: String
           , position :: String 
           , genotype :: String } deriving (Show)

-- initial parsing function
populate :: String -> [SNP]
populate str = map (\[a, b, c, d] -> SNP a b c d) $ process str
   where process = map words .
                       filter (\x -> head x /= '#') .
                       filter (\x -> not $ null x)
                       . lines

-- returns the percentage of SNPs that are of the supplied genotype
percentage :: [SNP] -> String -> Double
percentage xs pattern = (fromIntegral . length . filter (\x -> genotype x == pattern) $ xs)
                        / (fromIntegral . length $ xs)
                        * 100
