module Spec02 (spec) where

import Day02 (
    Hand (..),
    Result (..),
    hand,
    match,
    parse,
    score,
    scoreMatch,
 )
import Test.Hspec

spec :: SpecWith ()
spec = describe "Day 2 elves" $ do
    context "play Rock Scissors Paper where" $ do
        it "basic RSP rules work" $ do
            compare Rock Scissors `shouldBe` GT
            compare Paper Scissors `shouldBe` LT
            compare Paper Paper `shouldBe` EQ
            compare Paper Rock `shouldBe` GT
        it "letters to hands" $ do
            hand 'A' `shouldBe` Rock
            hand 'B' `shouldBe` Scissors
            hand 'C' `shouldBe` Paper
            hand 'X' `shouldBe` Rock
            hand 'Y' `shouldBe` Scissors
            hand 'Z' `shouldBe` Paper
            hand '0' `shouldBe` Wrong
        it "scores from hands" $ do
            score Rock `shouldBe` 1
            score Paper `shouldBe` 2
            score Scissors `shouldBe` 3
            score Wrong `shouldBe` 0
        it "scores from matches" $ do
            scoreMatch Win `shouldBe` 6
            scoreMatch Draw `shouldBe` 3
            scoreMatch Lose `shouldBe` 0
        it "I get proper scores" $ do
            match Rock Paper `shouldBe` 8
            match Paper Rock `shouldBe` 1
            match Scissors Scissors `shouldBe` 6
        it "translates strings to score" $ do
            parse "A Y\nB X\nC Z" `shouldBe` 15
