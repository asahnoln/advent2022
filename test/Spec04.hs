module Spec04 (spec) where

import Day04 (fullOverlaps)
import Test.Hspec

spec :: SpecWith ()
spec = do
    describe "Day 4 elves" $ do
        context "clean sectors" $ do
            it "find full overlaps of sectors" $ do
                fullOverlaps "2-4,6-8\n2-3,4-5\n5-7,7-9\n2-8,3-7\n6-6,4-6\n2-6,4-8" `shouldBe` 2
