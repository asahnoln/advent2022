module Day05 (
    performOn,
    move,
    parseCrates,
    parseCmds,
    Move (..),
    prepare,
    logPerformOn,
    perform9001On,
    move9001,
) where

import Data.Char (isSpace)

-- | Count of the crates to move.
type Count = Int

-- | Index for the column to move the crates from.
type From = Int

-- | Index for the column to move the crates to.
type To = Int

-- | Command type to use in move function.
data Move = Move Count From To deriving (Show, Eq)

-- | Finds top crates for the given string of crates and commands.
performOn :: String -> String
performOn = basePerformOn move

-- | Finds top crates for the given string of crates and commands, saving the order for multiple crates moves.
perform9001On :: String -> String
perform9001On = basePerformOn move9001

basePerformOn :: (Move -> [String] -> [String]) -> String -> String
basePerformOn fMove xss = map headForEmpty final
  where
    (_, final) = last $ baseLogPerformOn fMove xss

    headForEmpty :: String -> Char
    headForEmpty "" = ' '
    headForEmpty (x : _) = x

logPerformOn :: String -> [(Move, [String])]
logPerformOn = baseLogPerformOn move

baseLogPerformOn :: (Move -> [String] -> [String]) -> String -> [(Move, [String])]
baseLogPerformOn fMove xss = zip (Move 0 0 0 : cmds) (reverse final)
  where
    (cratesString, cmdString) = prepare xss
    crates = parseCrates cratesString
    cmds = parseCmds cmdString
    final = scanr fMove crates (reverse cmds)

-- | Moves crates with given Move command for the given string of crates in reverse order.
move :: Move -> [String] -> [String]
move = baseMove True

-- | Moves crates with given Move command for the given string of crates saving the order.
move9001 :: Move -> [String] -> [String]
move9001 = baseMove False

baseMove :: Bool -> Move -> [String] -> [String]
baseMove rv (Move c f t) xss = result
  where
    rTo = xss !! (t - 1)
    rFrom = xss !! (f - 1)

    (crates, rFromFinal) = splitAt c rFrom
    rToFinal = (if rv then reverse crates else crates) ++ rTo

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

-- | Parses given string of crates into lists of letters.
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

-- | Parses given string of commands into list of Move.
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

-- | Breaks given string of crates and commands into tuple of (crates, commands).
prepare :: String -> (String, String)
prepare xs = (unlines . init $ crates, unlines . filter (/= "") $ cmds)
  where
    (crates, cmds) = break (== "") $ lines xs
