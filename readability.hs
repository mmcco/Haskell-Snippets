-- have to deal with ellipses
-- try splitting into words first
-- there is a problem in the fact that "i.e." and "e.g." are split in half by breakNext
-- the resulting "e." and "i." are common and cannot be pattern-matched
-- the above problem is tentatively fixed
-- however, it's splitting senteces in quotations before the closing quotation mark
-- I'm going to try to fix this by first breaking by word


import Data.List (isPrefixOf, isSuffixOf, intersect)
import Control.Applicative
import System.Environment (getArgs)
import Data.Monoid


sentences :: String -> [String]
sentences = filter (\x -> length x > 1) . map (trim " \n\t") . combine . words
    where combine = foldl (\acc sent -> if isBreak (mlast acc) && not (hasException (mlast acc) sent)
                                          then acc ++ [sent]
                                          else safeInit acc ++ [mlast acc ++  " " ++ sent]) []
          hasException xs ys = or $ isSuffixOf <$> ["Mr.", "Mrs.", "Dr.", "St.", "cf.", "eg.", "i.e.", "e.g."] <*> [xs]
          isBreak xs = or $ isSuffixOf <$> [".", "!", "?", ".\"",".'","!\"","!'","?\"","?'"] <*> [xs]


mlast :: Monoid a => [a] -> a
mlast [] = mempty
mlast xs = last xs


safeInit :: [a] -> [a]
safeInit [] = []
safeInit xs = init xs


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
