module Day07 (total) where

total :: String -> Integer
total x = result
  where
    cmds =
        "$ cd a\n\
        \$ ls\n\
        \dir e\n\
        \29116 f\n\
        \2557 g\n\
        \62596 h.lst\n\
        \$ cd e\n\
        \$ ls\n\
        \584 i\n"
    e =
        calcDir
            "584 i"
    a =
        calcDir
            "dir e\n\
            \29116 f\n\
            \2557 g\n\
            \62596 h.lst\n"

    result = parseCmds cmds

    parseCmds :: String -> Integer
    parseCmds ss = undefined -- sum [ a , e , e ]
    getSize :: String -> Integer
    getSize ss = r
      where
        info = head $ words ss
        r = if info == "dir" then 0 else read info

    calcDir :: String -> Integer
    calcDir cs = r
      where
        fs = lines cs
        r = sum $ map getSize fs

-- parseCmds :: String -> Integer
-- parseCmds cs = result
--   where
--     (_ : _ : fs) = lines cs
--     result =
--         sum
--             [ sum $ map getSize $ tail fs
--             , calcDir $ unlines ["cd", "ls", head fs]
--             ]
