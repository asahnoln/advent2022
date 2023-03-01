module Day02 (
    match,
    score,
    Hand (..),
    hand,
    Result (..),
    scoreMatch,
    parse,
    result,
    handFor,
    parseWithResults,
)
where

-- | 'Hand' identifies a type of gesture a player can use in the game
data Hand = Paper | Scissors | Rock deriving (Eq, Show)

-- | Hands can be compared to each other - as Rock beats Scissors, thus Rock is Greater Than Scissors, etc.
instance Ord Hand where
    Paper `compare` Paper = EQ
    Rock `compare` Rock = EQ
    Scissors `compare` Scissors = EQ
    Paper `compare` Rock = GT
    Rock `compare` Scissors = GT
    Scissors `compare` Paper = GT
    Rock `compare` Paper = LT
    Scissors `compare` Rock = LT
    Paper `compare` Scissors = LT

-- | 'Enum' for 'Hand' helps identify what is the next or previous Hand after or before given one according to the rules of the game.
instance Enum Hand where
    pred Paper = Rock
    pred Rock = Scissors
    pred Scissors = Paper

    toEnum 0 = Paper
    toEnum 1 = Scissors
    toEnum 2 = Rock
    toEnum _ = Paper

    fromEnum Paper = 0
    fromEnum Scissors = 1
    fromEnum Rock = 2

-- | 'Result' is the result of a match.
data Result = Lose | Draw | Win deriving (Eq, Show)

-- | 'hand' translates given char to a 'Hand'.
hand :: Char -> Either String Hand
hand 'A' = Right Rock
hand 'B' = Right Paper
hand 'C' = Right Scissors
hand 'X' = Right Rock
hand 'Y' = Right Paper
hand 'Z' = Right Scissors
hand _ = Left "Wrong character for hand"

-- | 'result' translates given char to a 'Result'.
result :: Char -> Either String Result
result 'X' = Right Lose
result 'Y' = Right Draw
result 'Z' = Right Win
result _ = Left "Wrong character for result"

-- | 'score' gives a score for a chosen 'Hand'.
score :: Hand -> Integer
score Rock = 1
score Paper = 2
score Scissors = 3

-- | 'scoreMatch' gives score for a given 'Result'.
scoreMatch :: Result -> Integer
scoreMatch Win = 6
scoreMatch Draw = 3
scoreMatch Lose = 0

-- | 'handFor' suggests what 'Hand' you have to choose to achieve a desirable 'Result' against a given 'Hand'.
handFor :: Hand -> Result -> Hand
handFor x y = case y of
    Draw -> x
    Lose -> pred x
    Win -> succ x

-- | 'match' gives full score for a match. Player's 'Hand' is right one.
match :: Hand -> Hand -> Integer
match x y = score y + scoreMatch r
  where
    r = case compare y x of
        GT -> Win
        EQ -> Draw
        LT -> Lose

-- | 'parse' parses given string into series of matches.
parse :: String -> Either String Integer
parse = parseCommon False

-- | 'parseWithResults' parses given string into series of hands and desired results.
parseWithResults :: String -> Either String Integer
parseWithResults = parseCommon True

-- | 'parseCommon' is a helper function to get the sum of matches scores.
parseCommon :: Bool -> String -> Either String Integer
parseCommon r xs = do
    matches <-
        mapM
            ( \x -> do
                lh <- hand $ head x

                let c = x !! 2
                rh <-
                    if r
                        then do
                            rslt <- result c
                            return $ handFor lh rslt
                        else hand c

                return $ match lh rh
            )
            (lines xs)
    return $ sum matches
