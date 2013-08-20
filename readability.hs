-- have to deal with ellipses
-- try splitting into words first
-- there is a problem in the fact that "i.e." and "e.g." are split in half by breakNext
-- the resulting "e." and "i." are common and cannot be pattern-matched
-- the above problem is tentatively fixed
-- however, it's splitting senteces in quotations before the closing quotation mark
-- I'm going to try to fix this by first breaking by word


import Data.List (break, isPrefixOf, isSuffixOf, intercalate)
import Control.Applicative
import System.Environment (getArgs)


sentences :: String -> [String]
sentences "" = []
sentences xs = filter (\x -> length x > 1) . map trim . combine . words $ xs
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


trim :: String -> String
trim = dropWhile isWhitespace . reverse . dropWhile isWhitespace . reverse
    where isWhitespace = flip elem " \t\n"


main = do
    args <- getArgs
    myFile <- readFile (head args)
    let mySentences = map (++ "\n***") . sentences $ myFile
    mapM_ putStrLn mySentences
    print $ "number of sentences from sentences: " ++ show (length mySentences)
    print (map length mySentences)
