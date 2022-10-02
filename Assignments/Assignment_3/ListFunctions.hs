{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Use foldr" #-}
module ListFunctions where

import Prelude hiding (or, and, elem, take, drop)

and :: [Bool] -> Bool
and [] = True
and (x:xs) = x && and xs

or :: [Bool] -> Bool
or [] = True
or (x:xs) = x || or xs

elem :: (Eq a) => a -> [a] -> Bool
elem el [] = False
elem el (x:xs) = el == x || elem el xs

drop :: Int -> [a] -> [a]
drop n [] = []
drop 0 xs = xs
drop n (x:xs) = drop (n-1) xs

take :: Int -> [a] -> [a]
take n [] = []
take 0 xs = []
take n (x:xs) = x : take (n-1) xs
