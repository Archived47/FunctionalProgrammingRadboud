> module FoldrFusion where
>
> import Prelude hiding (map)

The fusion law for foldr states that

IF, for all x,y:
  f (g x y) = h x (f y)
THEN
  f . foldr g e = foldr h (f e)

> map :: (a -> b) -> [a] -> [b]
> map f = foldr (\x xs -> f x : xs) []

----------------------------------------------
To prove:  foldr g e . map f = foldr (g . f) e

To show that foldr g e . map f = foldr (g . f) e, we can apply the fusion law using
  f ==> foldr g e
  g ==> \x xs -> f x : xs
  e ==> []
  h ==> g . f

Namely as follows:

  foldr g e . map f
              ----- rewrite map as foldr
= foldr g e . foldr (\x xs->f x : xs) []
  ---------------------------------------- foldr fusion
= foldr (g . f) (foldr (\x xs->f x : xs) e [])
                ------------------------------ definition of foldr
= foldr (g . f) e

Since the "THEN" part of the fusion law can only be applied if the "IF" part is true,
we need to show that for all x,y:
  
  f (g x y) = h x (f y)
              g (f (x (f y)))

Which is the case since:

  f (g x y) =
  foldr g e (f x : y) =
  foldr g e ((foldr g e x) : y) =
  g (foldr g e x) (foldr g e y) =
  g . foldr g e (x (foldr g e y)) =
  h x (f y)

--------------------------------------
To prove:  map (f . g) = map f . map g
  or equivalently:  foldr (\x xs -> f . g x : xs) [] = foldr (\x xs -> f x : xs) [] . map g

We apply the foldr-map fusion law using
  f ==> g
  g ==> \x xs -> f x : xs
  g . f ==> \x xs -> f . g x : xs
  e ==> []

So we get: 
  map f . map g =
  foldr (\x xs -> f x : xs) [] . map g =
  foldr (\x xs -> f . g x : xs) [] =
  map (f . g)

----------------------------------------------
To prove:  mconcat . concat = mconcat . map mconcat

