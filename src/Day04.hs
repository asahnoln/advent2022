{-# LANGUAGE ImportQualifiedPost #-}

module Day04 (fullOverlaps, partOverlaps) where

import Data.Either (fromRight)
import Data.List (intersect)
import Data.Text qualified as T
import Data.Text.Read qualified as R

-- | Find full overlaps of given nubmer ranges.
fullOverlaps :: String -> Integer
fullOverlaps = baseOverlaps True

-- | Find partial overlaps of given nubmer ranges.
partOverlaps :: String -> Integer
partOverlaps = baseOverlaps False

baseOverlaps :: Bool -> String -> Integer
baseOverlaps full = toInteger . length . filter id . map (overlaps full) . lines

overlaps :: Bool -> String -> Bool
overlaps full xs = result
  where
    packDash = T.pack "-"
    gs = T.splitOn (T.pack ",") $ T.pack xs
    g1 = T.splitOn packDash $ gs !! 0
    g2 = T.splitOn packDash $ gs !! 1

    l1 = toNumbers g1
    l2 = toNumbers g2
    i = l1 `intersect` l2
    result =
        if full
            then i == l1 || i == l2
            else not . null $ i

toNumbers :: [T.Text] -> [Integer]
toNumbers ts = fromRight [] rslt
  where
    rslt = do
        (start, _) <- R.decimal $ ts !! 0
        (end, _) <- R.decimal $ ts !! 1

        return [start .. end]
