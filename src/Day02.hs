module Day02 (
    match,
    score,
    Hand (..),
    hand,
    Result (..),
    scoreMatch,
)
where

data Hand = Wrong | Paper | Scissors | Rock deriving (Eq, Show)

instance Ord Hand where
    Paper `compare` Paper = EQ
    Rock `compare` Rock = EQ
    Scissors `compare` Scissors = EQ
    Wrong `compare` Wrong = EQ
    Paper `compare` Rock = GT
    Rock `compare` Scissors = GT
    Scissors `compare` Paper = GT
    _ `compare` Wrong = GT
    Rock `compare` Paper = LT
    Scissors `compare` Rock = LT
    Paper `compare` Scissors = LT
    Wrong `compare` _ = LT

data Result = Lose | Draw | Win

hand :: Char -> Hand
hand 'A' = Rock
hand 'B' = Scissors
hand 'C' = Paper
hand 'X' = Rock
hand 'Y' = Scissors
hand 'Z' = Paper
hand _ = Wrong

score :: Hand -> Integer
score Rock = 1
score Paper = 2
score Scissors = 3
score _ = 0

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
