module WhatType where

f0 :: (Char, Char) -> Bool
f0 (x,y)   = x == 'F' && y == 'P'

f1 :: [Char] -> [Char]
f1 s       = s ++ ", cruel world!"

f2 :: b -> (c, a) -> (a, b, c)
f2 x (y,z) = (z,x,y)

f3 :: Char -> Char
f3 ' '     = '_'
f3 c       = c

f4 :: [Char] -> [Char] -> [Char]
f4 x y
  | x == ""   = y
  | otherwise = x

f5 :: Bool -> a -> a -> (a, a)
f5 b x y   = if b then (x,y) else (y,x)

f6 :: a -> b -> a
f6 x       = \y -> x

f7 :: [Char] -> [Char]
f7         = ("Haskell" ++)
