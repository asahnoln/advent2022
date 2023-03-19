module Day06 (markerPos, messagePos) where

import Data.List (nub)

-- Finds position of the last character of the packet where 4 last characters all unique.
markerPos :: String -> Integer
markerPos = checkUniq 4

-- Finds position of the last character of the message where 14 last characters all unique.
messagePos :: String -> Integer
messagePos = checkUniq 14

checkUniq :: Integer -> String -> Integer
checkUniq = go 0
  where
    go :: Integer -> Integer -> String -> Integer
    go i msgLn ss = y
      where
        sub = take (fromInteger msgLn) ss
        y =
            if nub sub == sub
                then i + msgLn
                else go (i + 1) msgLn (tail ss)
