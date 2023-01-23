{-# LANGUAGE ImportQualifiedPost #-}

module Day04 (fullOverlaps) where

import Data.List (intersect)
import Data.Text qualified as T
import Data.Text.Read qualified as R

fullOverlaps :: String -> Integer
fullOverlaps = sum . map overlaps . lines

overlaps :: String -> Integer
overlaps xs = result
  where
    gs = T.splitOn (T.pack ",") $ T.pack xs
    g1 = T.splitOn (T.pack "-") $ gs !! 0
    g2 = T.splitOn (T.pack "-") $ gs !! 1

    toNumbers :: [T.Text] -> [Integer]
    toNumbers ts = case rslt of
        Right x -> x
        Left _ -> []
      where
        rslt = do
            (start, _) <- R.decimal $ ts !! 0
            (end, _) <- R.decimal $ ts !! 1

            return [start .. end]

    l1 = toNumbers g1
    l2 = toNumbers g2
    i = l1 `intersect` l2
    result = if i == l1 || i == l2 then 1 else 0
