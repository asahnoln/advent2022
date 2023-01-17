module Main (main) where

import Lib (convertSringToCalories, findLargestCalory)

main :: IO ()
main = do
  l <- getContents
  print $ findLargestCalory $ convertSringToCalories l
