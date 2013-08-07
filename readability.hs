-- have to deal with ellipses
-- try splitting into words first

module Readability
( sentences,
  breakNext,
  breakPunc
) where

import Data.List (break, isSuffixOf, intercalate)
import Control.Applicative


sentences :: String -> [String]
sentences "" = []
sentences xs = map trim . combine . breakPunc $ xs
    where combine = foldl (\ys z -> if hasExceptions (safeLast ys) then (safeInit ys) ++ [(safeLast ys ++ z)] else ys ++ [z]) []
          hasExceptions xs = or $ pure isSuffixOf <*> ["Mr.", "Mrs.", "Dr.", "St.", "cf.", "eg.", "ie.", " i.", "i.e.", "e.", "e.g."] <*> pure xs


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


-- the first returned list contains everything up through the first contiguous set of "True" elements
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
    where isWhitespace = flip elem [' ', '\t', '\n']


main = do
    zenFile <- readFile "test.txt"
    let zenSentences =  sentences zenFile
    mapM_ putStrLn zenSentences
    print $ "number of sentences: " ++ show (length zenSentences)
