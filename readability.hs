-- have to deal with ellipses
-- try splitting into words first
-- there is a problem in the fact that "i.e." and "e.g." are split in half by breakNext
-- the resulting "e." and "i." are common and cannot be pattern-matched
-- the above problem is tentatively fixed
-- however, it's splitting senteces in quotations before the closing quotation mark
-- I'm going to try to fix this by first breaking by word


import Data.List (break, isPrefixOf, isSuffixOf, intercalate)
import Control.Applicative


sentences :: String -> [String]
sentences "" = []
sentences xs = filter (\x -> length x > 1) . map trim . combine . breakPunc $ xs
    where combine = foldl (\acc sent -> if hasException (lastStr acc) sent
                                          then safeInit acc ++ [lastStr acc ++ sent]
                                          else acc ++ [sent]) []
          -- takes two tentative sentences and determines whether they were split on non-sentence-ending punctuation
          hasException xs ys = or $ (isSuffixOf
                                   <$> ["Mr.", "Mrs.", "Dr.", "St.", "cf.", "eg.", "i.e.", "e.g."]
                                   <*> [xs])
                                 ++ ((\suffix prefix first second -> isSuffixOf suffix first && isPrefixOf prefix second)
                                   <$> ["i.", "e."]
                                   <*> ["e.", "g."]
                                   <*> [xs]
                                   <*> [ys])


newSent :: String -> [String]
newSent "" = []
newSent xs = filter (\x -> length x > 1) . map trim . combine . words $ xs
    where combine = foldl (\acc sent -> if isBreak (lastStr acc) && not (hasException (lastStr acc) sent)
                                          then acc ++ [sent]
                                          else safeInit acc ++ [lastStr acc ++  " " ++ sent]) []
          hasException xs ys = or $ [isSuffixOf] <*> ["Mr.", "Mrs.", "Dr.", "St.", "cf.", "eg.", "i.e.", "e.g."] <*> [xs]
          isBreak xs = or $ isSuffixOf <$> [".", "!", "?", ".\"",".'","!\"","!'","?\"","?'"] <*> [xs]


safeTail :: [a] -> [a]
safeTail [] = []
safeTail xs = tail xs


safeHead :: [String] -> String
safeHead [] = ""
safeHead xs = head xs


lastStr :: [String] -> String
lastStr [] = ""
lastStr strs = last strs


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
    where (this, next) = breakNext (`elem` ".?!") xs


trim :: String -> String
trim = dropWhile isWhitespace . reverse . dropWhile isWhitespace . reverse
    where isWhitespace = flip elem " \t\n"


main = do
    myFile <- readFile "test.txt"
    let mySentences = map (++ "\n***") . sentences $ myFile
    mapM_ putStrLn mySentences
    print $ "number of sentences: " ++ show (length mySentences)
    print (map length mySentences)
    let myNewSentences = map (++ "\n***") . newSent $ myFile
    mapM_ putStrLn myNewSentences
    print $ "number of sentences from newSent: " ++ show (length myNewSentences)
    print (map length mySentences)
