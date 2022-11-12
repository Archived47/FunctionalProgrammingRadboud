f xs = xs
g xs = xs

ys = map (f . g)
zs xs = map f (map g xs)

fs xs = (map (f . g)) xs


gs :: [[a]] -> [a]
gs xs = concat (map (map f) xs)
hs :: [[a]] -> [a]
hs xs = map f (concat xs)


foldrTest :: (a -> b -> b) -> b -> [a] -> b
foldrTest f b [] = b
foldrTest f b (a:as) = f a (foldrTest f b as)

compose :: [a -> a] -> a -> a
compose [] = id
compose (f:fs) = f . compose fs