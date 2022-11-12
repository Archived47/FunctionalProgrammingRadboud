> module Btree where
>
> data Btree a = Tip a | Bin (Btree a) (Btree a)
>
> mapBtree :: (a -> b) -> Btree a -> Btree b
> mapBtree f (Tip a)     = Tip (f a)
> mapBtree f (Bin t1 t2) = Bin (mapBtree f t1) (mapBtree f t2)

> tips :: (Btree a) -> [a]
> tips (Tip x) = [x]
> tips (Bin as bs) = tips as ++ tips bs

To prove: map f (tips t) = tips (mapBtree f t) for all f,t

Case 1: t = Tip x

    map f (tips (Tip x))
    ---------------------------   definition of tips
  = map f ([x])
  --                              definition of map
  = [(f x)]
  --                              specification tips
  = tips (Tip (f x))
  --                              specification mapBtree
  = tips (mapBtree f (Tip x))

Case 2: t = Bin l r
IH1: map f (tips l) = tips (mapBtree f l)
IH2: map f (tips r) = tips (mapBtree f r)

    map f (tips (Bin l r))
    --------------------------------------------  definition tips
  = map f (tips l ++ tips r)
  --                                              definition ++
  = map f (tips l) ++ map f (tips r)
  --                                              IH
  = tips (mapBtree f l) ++ tips (mapBtree f r)
  --                                              specification tips
  = tips (Bin (mapBtree f l) (mapBtree f r))
  --                                              specification mapBtree
  = tips (mapBtree f (Bin l r))