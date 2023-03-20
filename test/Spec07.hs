module Spec07 (spec) where

import Day07
import Test.Hspec

spec :: SpecWith ()
spec = do
    describe "Day 7 elves" $ do
        context "launch commands" $ do
            it "finds the total of all directories of size at most 100000" $ do
                total
                    "$ cd /\n\
                    \$ ls\n\
                    \dir a\n\
                    \14848514 b.txt\n\
                    \8504156 c.dat\n\
                    \dir d\n\
                    \$ cd a\n\
                    \$ ls\n\
                    \dir e\n\
                    \29116 f\n\
                    \2557 g\n\
                    \62596 h.lst\n\
                    \$ cd e\n\
                    \$ ls\n\
                    \584 i\n\
                    \$ cd ..\n\
                    \$ cd ..\n\
                    \$ cd d\n\
                    \$ ls\n\
                    \4060174 j\n\
                    \8033020 d.log\n\
                    \5626152 d.ext\n\
                    \7214296 k"
                    `shouldBe` 95437
