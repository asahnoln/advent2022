module Spec05 (spec) where

import Day05
import Test.Hspec

spec :: SpecWith ()
spec = do
    describe "Day 5 elves" $ do
        context "move crates" $ do
            context "parses string of commands" $ do
                it "1 command" $ do
                    parseCmds "move 1 from 2 to 3" `shouldBe` [Move 1 2 3]
                it "2 commands" $ do
                    parseCmds
                        "move 1 from 2 to 3\n\
                        \move 40 from 50 to 60"
                        `shouldBe` [Move 1 2 3, Move 40 50 60]
            context "parses string crates" $ do
                it "1 crate" $ do
                    parseCrates "[A]" `shouldBe` ["A"]
                it "2 crates" $ do
                    parseCrates "[A]\n[B]" `shouldBe` ["AB"]
                it "2 columns" $ do
                    parseCrates "[A] [C]" `shouldBe` ["A", "C"]
                it "2 columns and 2 rows" $ do
                    parseCrates "[A] [C]\n[B] [D]" `shouldBe` ["AB", "CD"]
                it "2 uneven columns and 2 rows" $ do
                    parseCrates "    [C]\n[B] [D]" `shouldBe` ["B", "CD"]
            it "moves crates" $ do
                move (Move 3 1 3) ["DNZ", "CM", "P"] `shouldBe` ["", "CM", "ZNDP"]
            it "moves crates in the same order" $ do
                move9001 (Move 3 1 3) ["DNZ", "CM", "P"] `shouldBe` ["", "CM", "DNZP"]
            it "break string into crates and commands" $ do
                prepare "[A]\n 1 \n\nmove 1 from 2 to 3"
                    `shouldBe` ("[A]\n", "move 1 from 2 to 3\n")
            context "get top crates" $ do
                it "for every column" $ do
                    performOn
                        "    [D]    \n\
                        \[N] [C]    \n\
                        \[Z] [M] [P]\n\
                        \ 1   2   3 \n\
                        \\n\
                        \move 1 from 2 to 1\n\
                        \move 3 from 1 to 3\n\
                        \move 2 from 2 to 1\n\
                        \move 1 from 1 to 2"
                        `shouldBe` "CMZ"
                it "make sure order of moves is proper" $ do
                    performOn
                        "    [D]    \n\
                        \[N] [C]    \n\
                        \[Z] [M] [P]\n\
                        \ 1   2   3 \n\
                        \\n\
                        \move 1 from 1 to 2\n\
                        \move 2 from 2 to 3"
                        `shouldBe` "ZCD"
                it "for some empty columns" $ do
                    performOn "[A] [B]\n 1  2 \n\nmove 1 from 1 to 2" `shouldBe` " A"
            context "CrateMover 9001" $ do
                it "moves multiple crates in the same order" $ do
                    perform9001On
                        "    [D]    \n\
                        \[N] [C]    \n\
                        \[Z] [M] [P]\n\
                        \ 1   2   3 \n\
                        \\n\
                        \move 1 from 2 to 1\n\
                        \move 3 from 1 to 3\n\
                        \move 2 from 2 to 1\n\
                        \move 1 from 1 to 2"
                        `shouldBe` "MCD"
            context "log" $ do
                it "logs moving crates" $ do
                    logPerformOn
                        "    [D]    \n\
                        \[N] [C]    \n\
                        \[Z] [M] [P]\n\
                        \ 1   2   3 \n\
                        \\n\
                        \move 1 from 2 to 1\n\
                        \move 3 from 1 to 3\n\
                        \move 2 from 2 to 1\n\
                        \move 1 from 1 to 2"
                        `shouldBe` [ (Move 0 0 0, ["NZ", "DCM", "P"])
                                   , (Move 1 2 1, ["DNZ", "CM", "P"])
                                   , (Move 3 1 3, ["", "CM", "ZNDP"])
                                   , (Move 2 2 1, ["MC", "", "ZNDP"])
                                   , (Move 1 1 2, ["C", "M", "ZNDP"])
                                   ]
