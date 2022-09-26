module Triangle where

triangle :: Int -> String
triangle n = tri 0 where
    tri :: Int -> String
    tri i = if n /= i then spaces ++ replicate (2 * i + 1) '*' ++ newline ++ tri (i + 1) else ""
        where
            spaces = replicate (n - i) ' '
            newline = "\n"

triangleIndent :: Int -> Int -> String
triangleIndent n indent = tri 0 where
    tri :: Int -> String
    tri i = if n /= i then spaces ++ replicate (2 * i + 1) '*' ++ newline ++ tri (i + 1) else ""
        where
            spaces = replicate (n - i + indent) ' '
            newline = "\n"

christmasTree :: Int -> String
christmasTree n  = tree 0 where
    tree m 
        | m >= 0 && m <= n = triangleIndent m (n - m) ++ tree (m + 1)
        | otherwise = ""