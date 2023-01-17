module Lib where

import Data.List (groupBy)

countCalories :: [Integer] -> Integer
countCalories = sum

findLargestCalory :: [[Integer]] -> Integer
findLargestCalory xss = maximum $ map countCalories xss

convertFileToCalories :: String -> [[Integer]]
convertFileToCalories xs = map (map read) ys
  where
    gs = groupBy (\x y -> x /= "" && y /= "") $ lines xs
    ys = filter (/= [""]) gs
