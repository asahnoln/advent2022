module Spec01 (spec) where

import Day01 (
    convertSringToCalories,
    findLargestCalory,
    findLargestCaloryOfTop3,
 )
import Test.Hspec

spec :: SpecWith ()
spec = describe "Day 1 elves" $ do
    it "can find biggest calory" $ do
        findLargestCalory [[10, 20, 30], [40], [50, 60]] `shouldBe` 110
    it "can convert strings of strings to list of int lists" $ do
        convertSringToCalories "4\n5\n6\n\n1\n\n2\n3" `shouldBe` [[4, 5, 6], [1], [2, 3]]
    it "can find biggest calory of top 3 elves" $ do
        findLargestCaloryOfTop3 [[1, 2], [3, 4], [5, 6], [7, 8]] `shouldBe` 33
