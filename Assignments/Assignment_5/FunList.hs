module FunList where

--define using the _list design pattern_
compose :: [a -> a] -> (a -> a)
compose [] = id
compose (x:xs) = x . compose xs

--define using `foldr`
compose' :: [a -> a] -> (a -> a)
compose' = foldr (.) id

--Explain _what_ the following function computes, and _how_ it computes it
{-
  It computes the multiplication of all the numbers between 1 and n
  compose (map (*) [1..n]) 1 => compose ([(\x -> x * 1), (\x -> x * 2), .. n]) 1
  So it then applies all these functions to the new result, with the first doing it on 1
  So this becomes 1 * 1 * 2 * 3 * 4 * .. * n
-}
foo :: (Integral n) => n -> n
foo n = compose (map (*) [1..n]) 1

--define in terms of *only* `map` and `compose`
foldr' :: (a -> b -> b) -> b -> [a] -> b
foldr' func input xs = compose (map func xs) input
