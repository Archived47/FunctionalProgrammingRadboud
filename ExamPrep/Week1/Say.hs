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
say 20 = "twenty"
say 30 = "thirty"
say 40 = "forty"
say 50 = "fifty"
say n 
  | n >= 1000 say thousand
  | otherwise = say 0
  


{-
	When we have 123456, we need to say "one hundred twenty three thousand four hundred fifty six"
	We can split this up in the groups of mor ethan a thousand and the group of smaller than a thousand
-}

