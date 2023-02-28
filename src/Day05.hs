module Day05 (performOn, move, parseCrates, parseCmds, Move (..)) where

import Data.Char (isSpace)

performOn :: String -> String
performOn xss = result
  where
    -- TODO: Data source from xss
    crates = ["NZ", "DCM", "P"]
    iss = [move 1 1 2, move 2 2 1, move 3 1 3, move 1 2 1]
    final = foldr (\f acc -> f acc) crates iss
    result = map head final

move :: Count -> From -> To -> [String] -> [String]
move c f t xss = result
  where
    rTo = xss !! (t - 1)
    rFrom = xss !! (f - 1)

    (crates, rFromFinal) = splitAt c rFrom
    rToFinal = reverse crates ++ rTo

    result =
        zipWith
            ( \i x ->
                if i == t
                    then rToFinal
                    else
                        if i == f
                            then rFromFinal
                            else x
            )
            [1 ..]
            xss

parseCrates :: String -> [String]
parseCrates cs = cratesToListOfStrings 1
  where
    ls = lines cs
    lineLength = length $ head ls

    cratesToListOfStrings :: Int -> [String]
    cratesToListOfStrings x
        | x > lineLength = []
        | otherwise = charList : cratesToListOfStrings (x + 4)
      where
        charList = filter (not . isSpace) . map (!! x) $ ls

parseCmds :: String -> [Move]
parseCmds = map getCmd . lines
  where
    getCmd :: String -> Move
    getCmd l = Move c f t
      where
        args = words l
        c = read $ args !! 1
        f = read $ args !! 3
        t = read $ args !! 5

type Count = Int
type From = Int
type To = Int

data Move = Move Count From To deriving (Show, Eq)
