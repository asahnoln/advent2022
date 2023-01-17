module Main (main) where

import Day01 (
    convertSringToCalories,
    findLargestCaloryOfTop3,
 )

main :: IO ()
main = do
    l <- getContents
    print $ findLargestCaloryOfTop3 $ convertSringToCalories l
