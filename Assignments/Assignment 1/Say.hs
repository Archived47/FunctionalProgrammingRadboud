module Say where

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
say 40 = "fourty"
say 50 = "fifty"
say 60 = "sixty"
say 70 = "seventy"
say 80 = "eighty"
say 90 = "ninety"

-- say 100 => one hundred (1 100)
-- <= million

-- `mod` 10 => get first digit
-- `mod` 100 => get second digit
-- `mod` 1000 => get third digit
-- `mod` 10000 => get fourth digit
-- `mod` 100000 => get fifth digit
-- `mod` 1000000 => get sixth digit

say n
    | n > 12 && n < 20 = say first ++ "teen"
    | n < 100 = say (second * 10) ++ " " ++ say first
    | n < 1000 = say third ++ " hundred " ++ say (n `mod` 100)
    | n < 1000000 = say fourth ++ " thousand " ++ say (n `mod` 1000)
    | otherwise = say n
    where 
        first = n `mod` 10
        second = n `mod` 100 `div` 10
        third = n `mod` 1000 `div` 100
        fourth = n `mod` 10000 `div` 1000
        fifth = n `mod` 100000 `div` 10000
        sixth = n `mod` 1000000 `div` 100000
        



-- 999999 => (nine hundred) (ninety) (nine) (thousand) (nine hundred) (ninety) (nine)