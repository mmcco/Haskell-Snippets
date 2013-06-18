import System.Directory (getDirectoryContents)
import Data.List (isSuffixOf)

main = do
          dirContents <- getDirectoryContents "/home/mike/jekyll"
          let markdowns = map ("/home/mike/jekyll/" ++ ) . filter (isSuffixOf ".md") $ dirContents
          files <- sequence . map readFile $ markdowns
          let categories = map ((flip (!!)) 1 . filter (\x -> head x == "category:") . map words . takeWhile ((/=) "---") . drop 1 . dropWhile ((/=) "---") . lines) files
              --parseMe = zip markdowns categories
          --print . unlines . map (\x -> fst x ++ " " ++ snd x) $ parseMe
          print "compiled"
