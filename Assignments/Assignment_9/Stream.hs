module Stream where

import qualified Data.List as List
import Prelude hiding (head, tail, repeat, map, zipWith, filter, take, drop, concat, cycle, sum)

data Stream a = a :> Stream a
infixr 5 :>

instance (Show a) => Show (Stream a) where
  show s = "(" List.++ showN (16::Int) s List.++ ")"
    where
    showN 0 _         = "..."
    showN n (x :> xs) = show x List.++ " :> " List.++ showN (n-1) xs

{-
  3. We get an empty stream, which show will never terminate.
-}

from :: Integer -> Stream Integer
from n = n :> from (n + 1)

head :: Stream a -> a
head (a :> b) = a

tail :: Stream a -> Stream a
tail (a :> b) = b

repeat :: a -> Stream a
repeat x = x :> repeat x

map :: (a -> b) -> (Stream a -> Stream b)
map f (a :> b) = f a :> map f b

zipWith :: (a -> b -> c) -> (Stream a -> Stream b -> Stream c)
zipWith f (a :> as) (b :> bs) = f a b :> zipWith f as bs

filter :: (a -> Bool) -> Stream a -> Stream a
filter f (a :> as) = if f a then a :> filter f as else filter f as

toList :: Stream a -> [a]
toList (a :> as) = a : toList as

cycle :: [a] -> Stream a
cycle xs = helper xs xs
  where
    helper :: [a] -> [a] -> Stream a
    helper xs [] = helper xs xs
    helper xs (y:ys) = y :> helper xs ys -- Turn y into the stream

nat, fib :: Stream Integer
nat = 0 :> zipWith (+) nat (repeat 1)
fib = 0 :> 1 :> zipWith (+) fib (tail fib)

primetwins :: Stream (Integer,Integer)
primetwins = primetwins' primes
  where
    primes :: Stream Integer
    primes = 2 :> sieve [3,5..]
      where
        sieve :: [Integer] -> Stream Integer
        sieve (x : xs) = x :> sieve [ y | y <- xs , y `mod` x /= 0]
    primetwins' :: Stream Integer -> Stream (Integer, Integer)
    primetwins' (x :> y :> s)
      | y - x == 2 = (x, y) :> primetwins' (y :> s)
      | otherwise = primetwins' (y :> s)

combine :: Stream a -> Stream a -> Stream a
combine (x :> xs) (y :> ys) = x :> y :> combine xs ys
