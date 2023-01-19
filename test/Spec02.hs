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
            hand 'A' `shouldBe` Right Rock
            hand 'B' `shouldBe` Right Paper
            hand 'C' `shouldBe` Right Scissors
            hand 'X' `shouldBe` Right Rock
            hand 'Y' `shouldBe` Right Paper
            hand 'Z' `shouldBe` Right Scissors
        it "scores from hands" $ do
            score Rock `shouldBe` 1
            score Paper `shouldBe` 2
            score Scissors `shouldBe` 3
        it "scores from matches" $ do
            scoreMatch Win `shouldBe` 6
            scoreMatch Draw `shouldBe` 3
            scoreMatch Lose `shouldBe` 0
        it "I get proper scores" $ do
            match Rock Paper `shouldBe` 8
            match Paper Rock `shouldBe` 1
            match Scissors Scissors `shouldBe` 6

            match Rock Rock `shouldBe` 4
            match Rock Scissors `shouldBe` 3
            match Scissors Rock `shouldBe` 7
            match Scissors Paper `shouldBe` 2
            match Paper Scissors `shouldBe` 9
            match Paper Paper `shouldBe` 5
        it "translates strings to score" $ do
            parse "A Y\nB X\nC Z" `shouldBe` Right 15
        it "translates wrong input to error" $ do
            parse "1 2\n3 4\n5 6" `shouldBe` Left "Wrong character"
