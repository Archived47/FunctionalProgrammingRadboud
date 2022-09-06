module Say where

type Group = (AboveThousand, BelowThousand)

type AboveThousand = Integer
type BelowThousand = Integer

-- toGroups --> get the group before thousand and after thousand
toGroups :: Integer -> Group
toGroups n = if n >= 10^6 then undefined else (n `div` 1000, n `mod` 1000)

condiftionalSay :: Integer -> String
condiftionalSay n = if n > 0 then " " ++ say n else ""

say :: Integer -> String
say  0 = "zero"
say  1 = "one"
say  2 = "two"
say  3 = "three"
say  4 = "four"
say  5 = "five"
say  6 = "six"
say  7 = "seven"
say  8 = "eight"
say  9 = "nine"
say 10 = "ten"
say 11 = "eleven"
say 12 = "twelve"
say 13 = "thirteen"
say 15 = "fifteen"
say 18 = "eighteen"
say 20 = "twenty"
say 30 = "thirty"
say 40 = "forty"
say 50 = "fifty"
say 60 = "sixty"
say 70 = "seventy"
say 80 = "eighty"
say 90 = "ninety"

say n
    | n > 12 && n < 20 = say (n `mod` 10) ++ "teen"
    | n < 100 = say ((n `mod` 100 `div` 10) * 10) ++ condiftionalSay (n `mod` 10)
    | n < 1000 = say (n `mod` 1000 `div` 100) ++ " hundred" ++ condiftionalSay (n `mod` 100)
    | otherwise = say above ++ " thousand" ++ condiftionalSay below
        where 
            (above, below) = toGroups n

-- 999999 => (nine hundred) (ninety) (nine) (thousand) (nine hundred) (ninety) (nine)
--        => (group 1) (thousand) (group 2)