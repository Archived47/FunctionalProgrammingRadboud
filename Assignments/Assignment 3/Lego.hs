module Lego where

import Data.List ( sort, sortBy )

removeAt :: Int -> [a] -> [a]
removeAt n [] = []
removeAt n xs = [x | (i, x) <- zip [1..] xs, i /= n]

sortWithPos :: (Ord a) => [a] -> [(a,Int)]
sortWithPos [] = []
sortWithPos xs = [(x,i) | (x,i) <- sortBy sortHelper indexPairs]
  where
    indexPairs = [(x, i) | (i, x) <- zip [0..] xs]
    sortHelper (a1, b1) (a2, b2)
      | a1 < a2 = LT
      | a1 > a2 = GT
      | otherwise = compare a1 a2
-- sortWithPos xs = [(x, j) | (x, i) <- sortedWithIndex, j <- find x xs]
--   where 
--     sortedWithIndex = [(x, i) | (i, x) <- zip [0..] (sort xs)]
--     find y xs = [i | (i, x) <- zip [0..] xs, y == x]

sortedPos :: (Ord a) => [a] -> [(a,Int)]
sortedPos [] = []
sortedPos xs = [(x, j) | x <- xs, (_, i) <- sortedWithIndex, j <- find x sortedWithIndex, i == j]
  where 
    sortedWithIndex = [(x, i) | (i, x) <- zip [0..] (sort xs)]
    find y xs = [i | (i, x) <- zip [0..] (map fst xs), y == x]
