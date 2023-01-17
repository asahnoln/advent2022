module Main (main) where

import Lib (convertFileToCalories, findLargestCalory)

main :: IO ()
main = do
  l <- getContents
  print $ findLargestCalory $ convertFileToCalories l
