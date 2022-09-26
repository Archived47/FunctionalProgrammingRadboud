module Pow2 where

-- pow2 :: Integer -> Integer
-- pow2 0 = 1
-- pow2 n = 2 * pow2 (n-1)


--after finishing the function, change the type to: 
--pow2 :: (Ord n, Num n, Num a) => n -> a
pow2 :: (Ord n, Num n, Num a) => n -> a
pow2 0 = 1
pow2 n = 2 * pow2 (n-1)
