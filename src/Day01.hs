module Day01 (findLargestCalory, convertSringToCalories, findLargestCaloryOfTop3) where

import Data.List (groupBy, sort)

{- | `findLargestCalory` finds largest calory sum from the list of calories
>>> findLargestCalory [[1, 2, 3], [5, 6], [4, 1]]
11 -- From the second list [5, 6]
-}
findLargestCalory :: [[Integer]] -> Integer
findLargestCalory xss = maximum $ map sum xss

{- | `findLargestCaloryOfTop3` finds the sum of top 3 largest calory sums from the list of calories
>>> findLargestCaloryOfTop3 [[1, 2], [5, 5], [4, 1], [2, 4]]
21 -- From the last 3 lists
-}
findLargestCaloryOfTop3 :: [[Integer]] -> Integer
findLargestCaloryOfTop3 xss = sum $ take 3 $ reverse $ sort $ map sum xss

{- | `convertSringToCalories` converts given strings with newline to list of calories.
>>> convertSringToCalories "1\\n2\\n3\\n\\n4\\n\\n5\\n6"
[[1, 2, 3], [4], [5, 6]]
-}
convertSringToCalories :: String -> [[Integer]]
convertSringToCalories xs = map (map read) ys
  where
    gs = groupBy (\x y -> x /= "" && y /= "") $ lines xs
    ys = filter (/= [""]) gs
