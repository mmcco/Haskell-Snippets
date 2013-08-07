-- have to deal with ellipses

import Data.List (break, isSuffixOf, intercalate)
import Control.Applicative


sentences :: String -> [String]
sentences "" = []
sentences xs = if hasExceptions fullLine
                 then trim (fullLine ++ safeHead theRest) : (safeTail theRest)
                 else trim fullLine : theRest
    where isPunctuation = flip elem ['.', '?', '!']
          (line, rest) = break isPunctuation xs
          punctuation = takeWhile isPunctuation rest
          fullLine = line ++ punctuation
          theRest = sentences (dropWhile isPunctuation rest)
          hasExceptions xs = or $ pure isSuffixOf <*> ["Mr.", "Mrs.", "Dr.", "St.", "cf.", "eg.", "ie.", "i.e.", "e.g."] <*> pure xs


safeTail :: [a] -> [a]
safeTail [] = []
safeTail [x] = []
safeTail xs = tail xs


safeHead :: [String] -> String
safeHead [] = ""
safeHead xs = head xs


trim :: String -> String
trim = dropWhile isWhitespace . reverse . dropWhile isWhitespace . reverse
    where isWhitespace = flip elem [' ', '\t', '\n']


main = do
    zenFile <- readFile "zen.txt"
    let zenSentences =  sentences zenFile
    mapM_ putStrLn zenSentences
    print $ "number of sentences: " ++ show (length zenSentences)
