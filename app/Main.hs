module Main where

import System.Environment (getArgs)
import Text.Show.Pretty

join :: String -> [String] -> String
join _ [] = []
join _ [x] = x
join sep (x : xs) = x ++ sep ++ join sep xs

qsort :: Ord a => [a] -> [a]
qsort [] = []
qsort (x : xs) = qsort (left) ++ [pivot] ++ qsort (right)
  where
    pivot = x
    left = filter (< pivot) xs
    right = filter (>= pivot) xs

main :: IO ()
main = do
  args <- getArgs
  contentString <- readFile $ args !! 0

  let sortedLines = qsort $ lines contentString
  let sortedContents = join "," sortedLines

  putStrLn $ ppShow sortedLines
  putStrLn sortedContents
