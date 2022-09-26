module Uniq where

uniq :: (Eq a) => [a] -> [a]
uniq [] = []
uniq (x1:x2:xs) = if x1 == x2 then x1 : uniq xs else x1 : uniq (x2 : xs)
uniq (x:xs) = x : uniq xs
