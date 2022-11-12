> module ListInduction where

-----------------------------------------------------
To prove: map (f . g) xs = map f (map g xs)
By induction on xs.

Case 1: xs = []

    map (f . g) []
    ----------------  definition of .
  = (map f . map g) []
    --                definition of .
  = map f (map g [])

Case 2: xs = (a:as)
IH: map (f . g) as = map f (map g as), for all f and g

    map (f . g) (a:as)
    ----------------  definition of .
  = (map f . map g) (a:as)
    --                IH
  = map f (map g (a:as))


-----------------------------------------------------
To prove: map f (as ++ bs) = (map f as) ++ (map f bs)
By induction on as

Case 1: as = []

    map f ([] ++ bs)
    ----------------  definition of ++
  = map f (bs)
    --                definition of ++
  = [] ++ (map f bs)
    --                definition of ++
  = (map f []) ++ (map f bs)

Case 2: as = (x:xs)
IH: map f (xs ++ bs) = (map f xs) ++ (map f bs), for all f

    map f ((x:xs) ++ bs)
    ------------------------------------  definition ++, definition of map
  = f x : map f (xs ++ bs)
    --                                    IH
  = f x : (map f xs) ++ (map f bs)
    --                                    definition of map
  = (map f (x:xs)) ++ (map f bs)

-----------------------------------------------------
xs has to be a list of lists: [[a]] because concat :: [[a]] -> [a]

To prove: concat (map (map f) xs) = map f (concat xs)

Case 1: xs = []

    concat (map (map f) [])
    --------------------------------  definition of map
  = concat []
  --                                  definition of concat
  = []
  --                                  specification of map
  = map f []
  --                                  specification of concat
  = map f (concat [])

Case 2: xs = (a:as)
IH: concat (map (map f) as) = map f (concat as)

    concat (map (map f) (a:as))
    --------------------------------  definition of map
    = concat (map f a : (map (map f) as))
    --                                definition of map, definition of ++, definition of list notation
    = map f a ++ concat ((map (map f) as))
    --                                IH
    = map f a ++ map f (concat as)
    --                                definition of map, definition of ++
    = map f (concat (a:as))
