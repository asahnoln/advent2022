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

-- | `Hand` identifies a type of gesture a player can use in the game
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

data Result = Lose | Draw | Win deriving (Eq, Show)

hand :: Char -> Either String Hand
hand 'A' = Right Rock
hand 'B' = Right Paper
hand 'C' = Right Scissors
hand 'X' = Right Rock
hand 'Y' = Right Paper
hand 'Z' = Right Scissors
hand _ = Left "Wrong character for hand"

result :: Char -> Either String Result
result 'X' = Right Lose
result 'Y' = Right Draw
result 'Z' = Right Win
result _ = Left "Wrong character for result"

score :: Hand -> Integer
score Rock = 1
score Paper = 2
score Scissors = 3

scoreMatch :: Result -> Integer
scoreMatch Win = 6
scoreMatch Draw = 3
scoreMatch Lose = 0

handFor :: Hand -> Result -> Hand
handFor x y = case y of
    Draw -> x
    Lose -> pred x
    Win -> succ x

match :: Hand -> Hand -> Integer
match x y = score y + scoreMatch r
  where
    r = case compare y x of
        GT -> Win
        EQ -> Draw
        LT -> Lose

parse :: String -> Either String Integer
parse = parseCommon False

parseWithResults :: String -> Either String Integer
parseWithResults = parseCommon True

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
