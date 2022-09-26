module Lego where

import Data.List ( sort, sortBy )

removeAt :: Int -> [a] -> [a]
removeAt n [] = []
removeAt n xs = [x | (i, x) <- zip [1..] xs, i /= n]

sortWithPos :: (Ord a) => [a] -> [(a,Int)]
sortWithPos [] = []
sortWithPos xs = sortBy (\(a,_) (b,_) -> compare a b) [(x, i) | (i, x) <- zip [0..] xs]

sortedWithIndex :: (Ord a, Num b, Enum b) => [a] -> [(a, b)]
sortedWithIndex xs = [(x, i) | (i, (_, x)) <- sortBy (\(_,(a,_)) (_,(b,_)) -> compare a b) (zip [0..] (sortBy (\(_,a) (_,b) -> compare a b) (zip [0..] xs)))]
