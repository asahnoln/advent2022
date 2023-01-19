module Day03 (sumDups, sumBadges) where

import Data.Char (isUpper, ord)
import Data.List (intersect)

-- TODO: Everything here is not safe. Things might blow up on wrong input.

-- | `sumDups` finds the sum of priorities of duplicate items (letters) in each half of each line of the given string.
sumDups :: String -> Integer
sumDups = sum . map findDupScore . lines

-- | `sumBadges` finds the sum of priorities of badges - duplicate items (letter) in each 3 lines of the given string.
sumBadges :: String -> Integer
sumBadges = sumBadges' . lines

-- | `sumBadges'` is a helper function to count the sum of the badges.
sumBadges' :: [String] -> Integer
sumBadges' [] = 0
sumBadges' lss = i + sumBadges' (drop 3 lss)
  where
    ls = take 3 lss
    is = (ls !! 0) `intersect` (ls !! 1) `intersect` (ls !! 2)
    i = chrScore $ head is

-- | `baseLwr` is a base number substracted from lowercased character codes.
baseLwr :: Int
baseLwr = 96

-- | `baseUppr` is a base number substracted from lowercased character codes.
baseUppr :: Int
baseUppr = 38

-- | `chrScore` finds a score for a given character, subtracting a base number from its code.
chrScore :: Char -> Integer
chrScore x = toInteger $ ord x - base
  where
    base = if isUpper x then baseUppr else baseLwr

-- | `findDupScore` finds a score for a duplicate item (letter) in both halves of the given string.
findDupScore :: String -> Integer
findDupScore l = chrScore i
  where
    (x, y) = splitAt (length l `div` 2) l
    is = x `intersect` y
    i = head is
