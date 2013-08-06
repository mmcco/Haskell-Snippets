-- have to deal with ellipses

import Data.List (break, isSuffixOf)
import Control.Applicative


sentences :: String -> [String]
sentences "" = []
sentences xs = if hasExceptions fullLine
                 then (fullLine ++ safeHead theRest) : (safeTail theRest)
                 else fullLine : (safeTail theRest)
    where isPunctuation = (flip elem) ['.', '?', '!']
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


isSentence :: String -> Bool
isSentence "" = False
isSentence xs = (endsWithPunctuation xs) && (noExceptions xs)
    where endsWithPunctuation xs = or $ pure isSuffixOf <*> [".", "?", "!"] <*> pure xs
          noExceptions        xs = not . or $ pure isSuffixOf <*> ["Mr.", "Mrs.", "Dr.", "St.", "cf.", "eg.", "ie.", "i.e.", "e.g."] <*> pure xs


main = do
    print . length . sentences $ "This is a test. It really is a test? You're right!"
