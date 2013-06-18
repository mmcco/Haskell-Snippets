import System.Directory (getDirectoryContents)
import Data.List (isSuffixOf, isPrefixOf)

getCategory :: [String] -> String
getCategory [] = "Misc"
getCategory xs = (\x -> if length x < 2 then "Misc" else x !! 1) . words . head $ xs

main = do
          dirContents <- getDirectoryContents "/home/mike/jekyll"
          let markdowns = map ("/home/mike/jekyll/" ++ ) . filter (isSuffixOf ".md") $ dirContents
          files <- mapM readFile markdowns
          let categories = map (getCategory . filter (isPrefixOf "categories:") . takeWhile ((/=) "---") . drop 1 . dropWhile ((/=) "---") . lines) files
              parseMe = zip markdowns categories
          --print . unlines . map (\x -> fst x ++ " " ++ snd x) $ parseMe
          print . unwords $ categories
