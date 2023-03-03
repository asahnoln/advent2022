module Spec06 (spec) where

import Day06
import Test.Hspec

spec :: SpecWith ()
spec = do
    describe "Day 6 elves" $ do
        context "emit signals" $ do
            it "gets the number of last marker character" $ do
                markerPos "mjqjpqmgbljsphdztnvjfqwrcgsmlb" `shouldBe` 7
                markerPos "bvwbjplbgvbhsrlpgdmjqwftvncz" `shouldBe` 5
                markerPos "nppdvjthqldpwncqszvftbrmjlhg" `shouldBe` 6
                markerPos "nznrnfrfntjfmvfwmzdfjlvtqnbhcprsg" `shouldBe` 10
                markerPos "zcfzfwzzqfrljwzlrfnpqdbhtmscgvjw" `shouldBe` 11
