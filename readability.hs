import Data.List (isPrefixOf, isSuffixOf, intersect)
import Control.Applicative
import System.Environment (getArgs)
import Data.Monoid


sentences :: String -> [String]
sentences = filter (\x -> length x > 1) . map (trim " \n\t") . combine . words
    where combine = foldr (\sent acc -> if isBreak sent && not (isException sent)
                                          then sent : acc
                                          else (sent ++ " " ++ mhead acc) : safeTail acc)
                        []
          isException xs = any (`isSuffixOf` xs) ["Mr.", "Mrs.", "Dr.", "St.", "cf.", "eg.", "i.e.", "e.g."]
          isBreak xs = any (`isSuffixOf` xs) [".", "!", "?", ".\"",".'","!\"","!'","?\"","?'"]


mhead :: Monoid a => [a] -> a
mhead [] = mempty
mhead xs = head xs


safeTail :: [a] -> [a]
safeTail [] = []
safeTail xs = tail xs


-- removes the supplied items from the ends of the list
trim :: Eq a => [a] -> [a] -> [a]
trim junk = dropWhile (`elem` junk) . reverse . dropWhile (`elem` junk) . reverse


-- only returns real words (not dashes, ellipses, etc.) and removes punctuation like dashes from the ends of words
realWords :: String -> [String]
realWords = map (trim "-") . filter (\word -> length (intersect word alphabet) > 0) . words
    where alphabet = ['A'..'Z'] ++ ['a'..'z']


main = do
    args <- getArgs
    myFile <- readFile (head args)
    let mySentences = map (++ "\n***") . sentences $ myFile
    mapM_ putStrLn mySentences
    print $ "number of sentences from sentences: " ++ show (length mySentences)
    print (map length mySentences)
