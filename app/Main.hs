module Main (main) where

import Day01 (
    convertSringToCalories,
    findLargestCaloryOfTop3,
 )
import Day02 (parse, parseWithResults)
import Day03 (sumDups, sumBadges)
import Day04 (fullOverlaps)

main :: IO ()
main = day04

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
    print $ sumBadges l

day04 :: IO ()
day04 = do
    l <- getContents
    print $ fullOverlaps l
