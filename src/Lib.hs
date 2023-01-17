module Lib (findLargestCalory, convertSringToCalories) where

import Data.List (groupBy)

{- | `findLargestCalory` finds largest calory sum from the list of calories
>>> findLargestCalory [[1, 2, 3], [5, 6], [4, 1]]
11 -- From the second list [5, 6]
-}
findLargestCalory :: [[Integer]] -> Integer
findLargestCalory xss = maximum $ map sum xss

{- | `convertSringToCalories` converts given strings with newline to list of calories.
>>> convertSringToCalories "1\\n2\\n3\\n\\n4\\n\\n5\\n6"
[[1, 2, 3], [4], [5, 6]]
-}
convertSringToCalories :: String -> [[Integer]]
convertSringToCalories xs = map (map read) ys
  where
    gs = groupBy (\x y -> x /= "" && y /= "") $ lines xs
    ys = filter (/= [""]) gs
