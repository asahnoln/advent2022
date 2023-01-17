import Lib
import Test.Hspec

main :: IO ()
main = hspec $ do
  describe "Elves" $ do
    it "can count calories of one" $ do
      countCalories [1000, 2000, 3000] `shouldBe` 6000
    it "can find biggest calory" $ do
      findLargestCalory [[10, 20, 30], [40], [50, 60]] `shouldBe` 110
    it "can convert strings of strings to list of int lists" $ do
      convertFileToCalories "4\n5\n6\n\n1\n\n2\n3" `shouldBe` [[4, 5, 6], [1], [2, 3]]
