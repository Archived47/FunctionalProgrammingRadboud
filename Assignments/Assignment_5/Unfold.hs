module Unfold where

import Data.List (unfoldr)
import Prelude hiding (take,zip,(++))

-- define all the below funtions using `unfoldr`
bits :: Int -> [Int]
bits = unfoldr (\x -> if x == 0 then Nothing else Just (x `mod` 2, x `div` 2))

zip :: [a] -> [b] -> [(a,b)]
zip xs ys = unfoldr go seed
  where
    seed = (xs, ys)
    go (xs, []) = Nothing
    go ([], ys) = Nothing
    go (x:xs, y:ys) = Just ((x, y), (xs, ys))

take :: Int -> [a] -> [a]
take n xs = unfoldr go seed
  where
    seed = (n, xs)
    go (0, xs) = Nothing
    go (n, []) = Nothing
    go (n, x:xs) = Just(x, (n-1,xs))

primes :: [Integer]
primes = unfoldr go [2..] 
  where
    -- No need for empty list case, since we always put [2..]
    go (p:xs) = Just(p, [n | n <- xs, n `mod` p /= 0])

-- alternative implementation of `primes`:
primes' :: [Integer]
primes' = sieve [2..]
  where sieve (p:xs) = p : sieve [ n | n <- xs, n `mod` p /= 0 ]

apo :: (t -> Either [a] (a, t)) -> t -> [a]
apo f seed = case f seed of
  Left l       -> l
  Right (a,ns) -> a : apo f ns

(++) :: [a] -> [a] -> [a]
xs ++ ys = apo go xs
  where
    go [] = Left ys
    go (x:xs) = Right (x, xs)
  
insert :: (Ord a) => a -> [a] -> [a]
insert x = apo go
  where
    go [] = Left [x]
    go (y:ys) 
      | x <= y = Left (x:y:ys)
      | otherwise = Right (y,ys)

unfoldrApo :: (t -> Maybe (a, t)) -> t -> [a]
unfoldrApo f seed = apo go seed
  where
    go seed = case f seed of
      Nothing -> Left []
      Just x -> Right x

-- Optional!
-- apoUnfoldr :: (t -> Either [a] (a, t)) -> t -> [a]
