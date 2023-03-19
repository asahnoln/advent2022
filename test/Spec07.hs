module Spec07 (spec) where

import Day07
import Test.Hspec

spec :: SpecWith ()
spec = do
    describe "Day 7 elves" $ do
        context "launch commands" $ do
            it "finds the total of all directories of size at most 100000" $ do
                total
                    "$ cd /\
                    \$ ls\
                    \dir a\
                    \14848514 b.txt\
                    \8504156 c.dat\
                    \dir d\
                    \$ cd a\
                    \$ ls\
                    \dir e\
                    \29116 f\
                    \2557 g\
                    \62596 h.lst\
                    \$ cd e\
                    \$ ls\
                    \584 i\
                    \$ cd ..\
                    \$ cd ..\
                    \$ cd d\
                    \$ ls\
                    \4060174 j\
                    \8033020 d.log\
                    \5626152 d.ext\
                    \7214296 k"
                    `shouldBe` 95437
