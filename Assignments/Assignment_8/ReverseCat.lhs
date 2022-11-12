> module ReverseCat where
> import Prelude hiding (reverse)
>
> reverse :: [a] -> [a]
> reverse [] = []
> reverse (x:xs) = reverse xs ++ [x]
>
> reverse' :: [a] -> [a]
> reverse' xs = reverseCat xs []
>
> reverseCat :: [a] -> [a] -> [a]
> reverseCat [] ys = ys
> reverseCat (x:xs) ys = reverseCat xs (x:ys)

---------------------------------------------
To prove: reverseCat xs ys = reverse xs ++ ys
By induction on xs.

Case 1: xs = []

    reverseCat [] ys
    ---------------- definition of reverseCat
  = ys
    --               definition of ++
  = [] ++ ys         
    --               definition of reverse
  = reverse [] ++ ys

Case 2: xs = (a:as)
IH: reverseCat as bs = reverse as ++ bs, for all bs.

    reverseCat (a:as) ys
    ----------------  definition of reverseCat
  = reverseCat as (a:ys)
    --                IH
  = reverse as ++ (a ++ ys)
    --                applying associativity
  = (reverse as ++ a) ++ ys
    --                applying reverse
  = reverse (a ++ as) ++ ys
    --                applying ++
  = reverse (a:as) ++ ys


-----------------------------------------------------
To prove: reverse xs = reverse' xs

Case 1: xs = []

    reverse xs
    ----------------  definition of reverse
  = reverse []
    --                applying reverse
  = []
    --                definition of reverseCat
  = reverseCat [] []
    --                definition of reverse'
  = reverse' []

Case 2: xs = (a:as)
IH: reverse (a:as) = reverseCat (a:as) ys, if ys = []

    reverse (a:as)
    ----------------  IH
  = reverseCat (a:as) []
    --                definition of reverse'
  = reverse' (a:as)

