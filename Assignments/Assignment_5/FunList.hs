module FunList where

--define using the _list design pattern_
--compose :: [a -> a] -> (a -> a)

--define using `foldr`
--compose' :: [a -> a] -> (a -> a)

--Explain _what_ the following function computes, and _how_ it computes it
{-

 Your explanation here

-}
foo :: (Integral n) => n -> n
foo n = compose (map (*) [1..n]) 1

--define in terms of *only* `map` and `compose`
--foldr' :: (a -> b -> b) -> b -> [a] -> b
