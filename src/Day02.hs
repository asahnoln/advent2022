module Day02 (
    match,
    score,
    Hand (..),
    hand,
    Result (..),
    scoreMatch,
    parse,
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

data Result = Lose | Draw | Win

hand :: Char -> Either String Hand
hand 'A' = Right Rock
hand 'B' = Right Paper
hand 'C' = Right Scissors
hand 'X' = Right Rock
hand 'Y' = Right Paper
hand 'Z' = Right Scissors
hand _ = Left "Wrong character"

score :: Hand -> Integer
score Rock = 1
score Paper = 2
score Scissors = 3

scoreMatch :: Result -> Integer
scoreMatch Win = 6
scoreMatch Draw = 3
scoreMatch Lose = 0

match :: Hand -> Hand -> Integer
match x y = score y + scoreMatch result
  where
    result = case compare y x of
        GT -> Win
        EQ -> Draw
        LT -> Lose

parse :: String -> Either String Integer
parse xs = do
    matches <-
        mapM
            ( \x -> do
                lh <- hand $ x !! 0
                rh <- hand $ x !! 2
                return $ match lh rh
            )
            (lines xs)
    return $ sum matches
