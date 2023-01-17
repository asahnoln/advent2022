import Lib (convertSringToCalories, findLargestCalory)
import Test.Hspec

main :: IO ()
main = hspec $ do
  describe "Elves" $ do
    it "can find biggest calory" $ do
      findLargestCalory [[10, 20, 30], [40], [50, 60]] `shouldBe` 110
    it "can convert strings of strings to list of int lists" $ do
      convertSringToCalories "4\n5\n6\n\n1\n\n2\n3" `shouldBe` [[4, 5, 6], [1], [2, 3]]
