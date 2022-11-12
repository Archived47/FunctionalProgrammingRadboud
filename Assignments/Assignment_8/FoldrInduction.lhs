Local definitions:

> import Prelude hiding (foldr)

> foldr :: (a -> b -> b) -> b -> [a] -> b
> foldr f b [] = b
> foldr f b (x:xs) = f x (foldr f b xs)
>
> compose :: [a -> a] -> a -> a
> compose [] = id
> compose (f:fs) = f . compose fs

-----------------------------------------------------
To prove: foldr f b xs = compose (map f xs) b
By induction on xs.

Case 1: xs = []

    foldr f b []
    --------------------- definition of foldr
  = b
  --                      specification of compose
  = compose [] b
  --                      specification of map
  = compose (map f []) b

Case 2: xs = (a:as)
IH: foldr f b as = compose (map f as) b

    foldr f b (a:as)
    ----------------------------  definition of foldr
  = f a (foldr f b as)
  --                              IH
  = f a (compose (map f as) b)
  --                              definition of compose
  = compose (map f (a:as)) b
