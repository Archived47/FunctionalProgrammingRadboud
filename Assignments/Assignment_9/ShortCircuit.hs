module ShortCircuit where

andl, andr, orl, orr :: [Bool] -> Bool

andl = foldl (&&) True
andr = foldr (&&) True
orl  = foldl (||) False
orr  = foldr (||) False

e1, e2, e3, e4 :: Bool

e1 = andl $ False : [True,  True  ..]
e2 = andr $ False : [True,  True  ..]
e3 = orl  $ True  : [False, False ..]
e4 = orr  $ True  : [False, False ..]

{-
1. I expect that:
  andl: Will never end, as foldl is tail-recursive
  andr: Will return false, because it will instanly see False && True = False
  orl: Will never end, as foldl will never see the True
  orr: Will return true, as it sees True || False = True
-}

{-
2. Foldr is better, because it istantly checks if the first element and the next are true/false, just like with normal (&&) and (||)
-}
