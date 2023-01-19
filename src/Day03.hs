module Day03 (sumDups, sumBadges) where

import Data.Char (isUpper, ord)
import Data.List (intersect)

-- | `sumDups` finds duplicate items (letters) in each half of each line of the given string.
sumDups :: String -> Integer
sumDups = sum . map findDupScore . lines

sumBadges :: String -> Integer
sumBadges xs = chrScore i + chrScore i2
  where
    lss = lines xs
    ls = take 3 lss
    is = (ls !! 0) `intersect` (ls !! 1) `intersect` (ls !! 2)
    i = head is

    ls2 = drop 3 lss
    is2 = (ls2 !! 0) `intersect` (ls2 !! 1) `intersect` (ls2 !! 2)
    i2 = head is2

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
