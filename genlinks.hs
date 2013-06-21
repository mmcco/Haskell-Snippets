import System.Directory (getDirectoryContents)
import Data.List (isSuffixOf, isPrefixOf)

getCategory :: String -> String
getCategory file = if length (words headerLine) < 2 then "Misc." else (flip (!!)) 2 . words $ headerLine
    where headerLine = head . filter (isPrefixOf "category:") . lines $ file

getTitle :: String -> String
getTitle file = if length (words headerLine) < 2 then "Untitled" else (flip (!!)) 2 . words $ headerLine
    where headerLine = head . filter (isPrefixOf "title:") . lines $ file

main = do
          dirContents <- getDirectoryContents "/home/mike/jekyll"
          let markdowns = map ("/home/mike/jekyll/" ++ ) . filter (isSuffixOf ".md") $ dirContents
          files <- mapM readFile markdowns
          let tuples = map (\x -> (getTitle x, getCategory x)) files
          print tuples
