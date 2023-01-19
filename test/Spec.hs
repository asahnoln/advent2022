{-# LANGUAGE ImportQualifiedPost #-}

import Spec01 qualified
import Spec02 qualified
import Spec03 qualified
import Test.Hspec

main :: IO ()
main = hspec $ do
  Spec01.spec
  Spec02.spec
  Spec03.spec
