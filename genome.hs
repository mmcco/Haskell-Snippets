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

populate :: String -> [SNP]
populate str = map (\[a, b, c, d] -> SNP a b c d) $ process str
   where process str = map words . filter (\x -> head x /= '#') . filter (\x -> not $ null x) . lines $ str

percentage :: [SNP] -> String -> Double
percentage xs pattern = (fromIntegral . length . filter (\x -> genotype x == pattern) $ xs) / (fromIntegral . length $ xs)
                        * 100


