module Main (main) where

import Day01 (
    convertSringToCalories,
    findLargestCaloryOfTop3,
 )
import Day02 (parse)

main :: IO ()
main = day02

day01 :: IO()
day01 = do
    l <- getContents
    print $ findLargestCaloryOfTop3 $ convertSringToCalories l

day02 :: IO()
day02 = do
    l <- getContents
    print $ parse l

