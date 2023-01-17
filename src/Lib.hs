module Lib (findLargestCalory, convertSringToCalories) where

import Data.List (groupBy)

findLargestCalory :: [[Integer]] -> Integer
findLargestCalory xss = maximum $ map sum xss

convertSringToCalories :: String -> [[Integer]]
convertSringToCalories xs = map (map read) ys
  where
    gs = groupBy (\x y -> x /= "" && y /= "") $ lines xs
    ys = filter (/= [""]) gs
