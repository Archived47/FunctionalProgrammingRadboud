module EditDistance where

import Data.Array

naiveEditDistance :: String -> String -> Int
naiveEditDistance xs ys = distance xs ys
  where
    distance :: String -> String -> Int
    distance [] ys = length ys
    distance xs [] = length xs
    distance (x : xs) (y : ys) = minimum [1 + distance xs (y : ys), 1 + distance (x : xs) ys, cost x y + distance xs ys]

    cost :: (Eq a, Num c) => a -> a -> c
    cost x y = if x == y then 0 else 1

editDistance :: String -> String -> Int
editDistance xs ys = distArray ! (max_x, max_y)
  where
    max_x, max_y :: Int
    (max_x, max_y) = (length xs, length ys)

    x, y :: Array Int Char
    x = array (1, max_x) (zip [1 ..] xs)
    y = array (1, max_y) (zip [1 ..] ys)

    distArray :: Array (Int, Int) Int
    distArray = array ((0, 0), (max_x, max_y)) [(ij, distance ij) | i <- [0 .. max_x], j <- [0 .. max_y], let ij = (i, j)]

    distance :: (Int, Int) -> Int
    distance (0, j) = j
    distance (i, 0) = i
    distance (i, j) =
      minimum
        [ 
          distArray ! (i - 1, j) + 1,
          distArray ! (i, j - 1) + 1,
          if x ! i == y ! j then distArray ! (i - 1, j - 1) else 1 + distArray ! (i - 1, j - 1)
        ]