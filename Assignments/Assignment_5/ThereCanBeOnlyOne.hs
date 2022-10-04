module ThereCanBeOnlyOne where

-- onlyElem :: (Eq a) => a -> [a] -> Bool
-- onlyElem n xs = length (filter (== n) xs) == 1

-- onlyElem :: (Eq a) => a -> [a] -> Bool
-- onlyElem n = (== 1) . length . filter (== n)

onlyOnce :: (a -> Bool) -> [a] -> Bool
onlyOnce f = (== 1) . length . filter f

onlyElem :: (Eq a) => a -> [a] -> Bool
onlyElem n = onlyOnce (== n)
