{-# LANGUAGE ImportQualifiedPost #-}

import Spec01 qualified
import Spec02 qualified
import Spec03 qualified
import Spec04 qualified
import Spec05 qualified
import Spec06 qualified
import Spec07 qualified
import Test.Hspec

main :: IO ()
main = hspec $ do
    Spec01.spec
    Spec02.spec
    Spec03.spec
    Spec04.spec
    Spec05.spec
    Spec06.spec
    Spec07.spec
