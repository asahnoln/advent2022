module Day03 (sumDups) where

import Data.Char (isUpper, ord)
import Data.List (intersect)

baseLwr :: Int
baseLwr = 96

baseUppr :: Int
baseUppr = 38

chrScore :: Char -> Integer
chrScore x = toInteger $ ord x - base
  where
    base = if isUpper x then baseUppr else baseLwr

findDupScore :: String -> Integer
findDupScore l = chrScore i
  where
    (x, y) = splitAt (length l `div` 2) l
    is = x `intersect` y
    i = head is

sumDups :: String -> Integer
sumDups = sum . map findDupScore . lines
