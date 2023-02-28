module Spec05 (spec) where

import Day05
import Test.Hspec

spec :: SpecWith ()
spec = do
    describe "Day 5 elves" $ do
        context "move crates" $ do
            context "parses string crates" $ do
                it "1 crate" $ do
                    parseCrates "[A]" `shouldBe` ["A"]
                it "2 crates" $ do
                    parseCrates "[A]\n[B]" `shouldBe` ["AB"]
                it "2 columns" $ do
                    parseCrates "[A] [C]" `shouldBe` ["A", "C"]
                it "2 columns and 2 rows" $ do
                    parseCrates "[A] [C]\n[B] [D]" `shouldBe` ["AB", "CD"]
            it "moves crates" $ do
                move 3 1 3 ["DNZ", "CM", "P"] `shouldBe` ["", "CM", "ZNDP"]
            it "get top crates" $ do
                performOn
                    "    [D]    \
                    \[N] [C]    \
                    \[Z] [M] [P]\
                    \ 1   2   3 \
                    \\
                    \move 1 from 2 to 1\
                    \move 3 from 1 to 3\
                    \move 2 from 2 to 1\
                    \move 1 from 1 to 2"
                    `shouldBe` "CMZ"
