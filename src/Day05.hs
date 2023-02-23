module Day05 (performOn, move, parseCrates) where

performOn :: String -> String
performOn xss = result
  where
    -- TODO: Data source from xss
    crates = ["NZ", "DCM", "P"]
    iss = [move 1 1 2, move 2 2 1, move 3 1 3, move 1 2 1]
    final = foldr (\f acc -> f acc) crates iss
    result = map head final

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

parseCrates :: String -> [String]
parseCrates cs = ss
  where
    ls = lines cs
    -- FIX: Different line widths
    ss = [map (!! 1) ls, map (!! 5) ls]
