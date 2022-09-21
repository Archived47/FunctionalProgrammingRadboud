module Lego where

import Data.List ( sort, sortBy )

removeAt :: Int -> [a] -> [a]
removeAt n [] = []
removeAt n xs = [x | (i, x) <- zip [1..] xs, i /= n]

sortWithPos :: (Ord a) => [a] -> [(a,Int)]
sortWithPos [] = []
sortWithPos xs = sortBy (\(a,_) (b,_) -> compare a b) [(x, i) | (i, x) <- zip [0..] xs]

-- sortedPos :: (Ord a) => [a] -> [(a,Int)]
-- sortedPos [] = []
-- sortedPos xs = [(x, j) | x <- xs, (_, i) <- sortedWithIndex xs, j <- find x (sortedWithIndex xs), i == j]
--   where 
--     find y xs = [i | (i, x) <- zip [0..] (map fst xs), y == x]


-- sortedWithIndex :: (Num b, Enum b, Ord a) => [a] -> [(a, b)]
-- sortedWithIndex xs = [(x, i) | (i, x) <- zip [0..] (sort xs)]

sortedWithIndex :: (Num b, Enum b, Ord a) => [a] -> [(a, b)]
sortedWithIndex xs = [(x, i) | (i, x) <- zip [0..] (sort xs)]

main = sortedWithIndex "haskell"
