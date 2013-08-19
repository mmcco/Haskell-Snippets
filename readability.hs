-- have to deal with ellipses
-- try splitting into words first
-- there is a problem in the fact that "i.e." and "e.g." are split in half by breakNext
-- the resulting "e." and "i." are common and cannot be pattern-matched


import Data.List (break, isPrefixOf, isSuffixOf, intercalate)
import Control.Applicative


sentences :: String -> [String]
sentences "" = []
sentences xs = map trim . combine . breakPunc $ xs
    where combine = foldl (\ys z -> if hasException (safeLast ys) z then (safeInit ys) ++ [safeLast ys ++ z] else ys ++ [z]) []
          -- takes two tentative sentences and determines whether they were split on non-sentence-ending punctuation
          hasException xs ys = or $ (pure isSuffixOf <*> ["Mr.", "Mrs.", "Dr.", "St.", "cf.", "eg.", "i.e.", "e.g."] <*> pure xs)
                                    ++ (pure (\suffix prefix first second -> isSuffixOf suffix first && isPrefixOf prefix second) <*> ["i.", "e."] <*> ["e.", "g."] <*> pure xs <*> pure ys)


safeTail :: [a] -> [a]
safeTail [] = []
safeTail xs = tail xs


safeHead :: [String] -> String
safeHead [] = ""
safeHead xs = head xs


safeLast :: [String] -> String
safeLast [] = ""
safeLast xs = last xs


safeInit :: [a] -> [a]
safeInit [] = []
safeInit xs = init xs


-- the first returned list contains the first set of contiguous "True" elements and the following set of contiguous "False" elements
breakNext :: (a -> Bool) -> [a] -> ([a], [a])
breakNext f [] = ([], [])
breakNext f [x] = ([x], [])
breakNext f xs = (a ++ c, d)
    where (a, b) = break f xs
          (c, d) = span f b


-- returns a list of possible sentences broken at punctuation with breakNext
breakPunc :: String -> [String]
breakPunc "" = []
breakPunc xs = this : breakPunc next
    where (this, next) = breakNext (flip elem ".?!") xs


trim :: String -> String
trim = dropWhile isWhitespace . reverse . dropWhile isWhitespace . reverse
    where isWhitespace = flip elem " \t\n"


main = do
    myFile <- readFile "test.txt"
    let mySentences =  sentences myFile
    mapM_ putStrLn mySentences
    print $ "number of sentences: " ++ show (length mySentences)
    print (map length mySentences)
