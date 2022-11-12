> module FoldrFusion where
>
> import Prelude hiding (map)

The fusion law for foldr states that

for all
  f0 :: b -> c
  g0 :: a -> b -> b
  h0 :: a -> c -> c
  e0 :: b
IF, for all x :: a, y :: b.
  f0 (g0 x y) = h0 x (f0 y)
THEN
  f0 . foldr g0 e0 = foldr h0 (f0 e0)

> map :: (a -> b) -> [a] -> [b]
> map f = foldr (\x xs -> f x : xs) []

----------------------------------------------
To prove:  foldr g e . map f = foldr (g . f) e

To show that foldr g e . map f = foldr (g . f) e, we can apply the fusion law using
  f0 ==> foldr g e
  g0 ==> \x xs -> f x : xs
  e0 ==> []
  h0 ==> g . f

Namely as follows:

  foldr g e . map f
              ----- rewrite map as foldr
= foldr g e . foldr (\x xs->f x : xs) []
  ---------------------------------------- foldr fusion
= foldr (g . f) (foldr (\x xs->f x : xs) e [])
                ------------------------------ definition of foldr
= foldr (g . f) e

Since the "THEN" part of the fusion law can only be applied if the "IF" part is true,
we need to show that ..  

Which is the case since:

  ...

--------------------------------------
To prove:  map (f . g) = map f . map g



----------------------------------------------
To prove:  mconcat . concat = mconcat . map mconcat

