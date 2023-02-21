module Spec03 (spec) where

import Day03 (sumBadges, sumDups)
import Test.Hspec

spec :: SpecWith ()
spec = do
    describe "Day 3 elves" $ do
        context "check rucksacks" $ do
            it "sums all duplicates" $ do
                sumDups "vJrwpWtwJgWrhcsFMMfFFhFp\njqHRNqRjqzjGDLGLrsFMfFZSrLrFZsSL\nPmmdzqPrVvPwwTWBwg\nwMqvLMZHhHMvwLHjbvcjnnSBnvTQFn\nttgJtRGJQctTZtZT\nCrZsJsPPZsGzwwsLwLmpwMDw" `shouldBe` 157
            it "sums all badges (duplicates for every three lines)" $ do
                sumBadges "vJrwpWtwJgWrhcsFMMfFFhFp\njqHRNqRjqzjGDLGLrsFMfFZSrLrFZsSL\nPmmdzqPrVvPwwTWBwg\nwMqvLMZHhHMvwLHjbvcjnnSBnvTQFn\nttgJtRGJQctTZtZT\nCrZsJsPPZsGzwwsLwLmpwMDw" `shouldBe` 70
