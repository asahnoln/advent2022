module Main (main) where

import Day01 (
    convertSringToCalories,
    findLargestCaloryOfTop3,
 )
import Day02 (parse, parseWithResults)
import Day03 (sumDups)

main :: IO ()
main = day03

day01 :: IO ()
day01 = do
    l <- getContents
    print $ findLargestCaloryOfTop3 $ convertSringToCalories l

day02 :: IO ()
day02 = do
    l <- getContents
    print $ parseWithResults l

day03 :: IO ()
day03 = do
    l <- getContents
    print $ sumDups l
