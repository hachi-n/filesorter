module Main where

import qualified Data.List as List
import Data.Semigroup ((<>))
import Options.Applicative
import qualified Text.Show.Pretty as Pretty

data Options = Options
  { file :: String
  }

option' :: Parser Options
option' =
  Options
    <$> strOption
      ( long "file"
          <> short 'f'
          <> metavar "FILENAME"
          <> help "Target for the filename"
      )

main :: IO ()
main = do
  content <- loadFile =<< execParser opts

  let sortedLines = List.sort $ lines content
  let sortedContents = join "," sortedLines

  putStrLn $ Pretty.ppShow sortedContents
  where
    opts =
      info
        (option' <**> helper)
        ( fullDesc
            <> progDesc "Print a greeting for TARGET"
            <> header "hello - a test for optparse-applicative"
        )

loadFile :: Options -> IO (String)
loadFile (Options f) = readFile f

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
