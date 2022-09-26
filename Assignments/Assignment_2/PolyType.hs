module PolyType where

f8 :: Ord p => p -> p -> p
f8 x y  = if x <= y then x else y

f9 :: Bool -> Bool -> Bool
f9 x y  = not x || y

f10 :: (Eq a, Num a) => a -> a -> a
f10 x y
  | x == 0    = y
  | otherwise = x + y

f11 :: p -> p -> p
f11 x y = get 0
  where get n = if n == 0 then x else y
