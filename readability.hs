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


sentences :: String -> [String]
sentences xs = filter (\x -> length x > 1) . map (trim " \n\t") . combine . words $ xs
    where combine = foldl (\acc sent -> if isBreak (lastStr acc) && not (hasException (lastStr acc) sent)
                                          then acc ++ [sent]
                                          else safeInit acc ++ [lastStr acc ++  " " ++ sent]) []
          hasException xs ys = or $ isSuffixOf <$> ["Mr.", "Mrs.", "Dr.", "St.", "cf.", "eg.", "i.e.", "e.g."] <*> [xs]
          isBreak xs = or $ isSuffixOf <$> [".", "!", "?", ".\"",".'","!\"","!'","?\"","?'"] <*> [xs]


lastStr :: [String] -> String
lastStr [] = ""
lastStr strs = last strs


safeInit :: [a] -> [a]
safeInit [] = []
safeInit xs = init xs


-- removes the supplied items from the ends of the list
trim :: Eq a => [a] -> [a] -> [a]
trim junk = dropWhile (`elem` junk) . reverse . dropWhile (`elem` junk) . reverse


realWords :: String -> [String]
realWords = filter (\word -> length (intersect word alphabet) > 0) . words
    where alphabet = ['A'..'Z'] ++ ['a'..'z']


main = do
    args <- getArgs
    myFile <- readFile (head args)
    let mySentences = map (++ "\n***") . sentences $ myFile
    mapM_ putStrLn mySentences
    print $ "number of sentences from sentences: " ++ show (length mySentences)
    print (map length mySentences)
