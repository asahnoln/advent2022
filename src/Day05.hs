module Day05 (performOn, move) where

performOn :: String -> String
performOn _ = undefined

move :: Int -> Int -> Int -> [String] -> [String]
move c f t xss = result
  where
    rTo = xss !! (t - 1)
    rFrom = xss !! (f - 1)

    (crates, rFromFinal) = splitAt c rFrom
    rToFinal = reverse crates ++ rTo

    result =
        zipWith
            ( \i x ->
                if i == t
                    then rToFinal
                    else
                        if i == f
                            then rFromFinal
                            else x
            )
            [1 ..]
            xss
