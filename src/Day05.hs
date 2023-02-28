module Day05 (performOn, move, parseCrates, parseCmds, Move (..), prepare) where

import Data.Char (isSpace)

type Count = Int
type From = Int
type To = Int

data Move = Move Count From To deriving (Show, Eq)

performOn :: String -> String
performOn xss = map headForEmpty final
  where
    (cratesString, cmdString) = prepare xss
    crates = parseCrates cratesString
    cmds = parseCmds cmdString
    final = foldr move crates cmds

    headForEmpty :: String -> Char
    headForEmpty "" = ' '
    headForEmpty (x:_) = x

move :: Move -> [String] -> [String]
move (Move c f t) xss = result
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

prepare :: String -> (String, String)
prepare xs = (unlines . init $ crates, unlines . filter (/= "") $ cmds)
  where
    (crates, cmds) = break (== "") $ lines xs
