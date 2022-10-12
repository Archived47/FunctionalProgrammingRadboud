module Pronounceable where

import Data.List
import Say (say)

class Pronounceable a where
  pronounce :: a -> String

instance Pronounceable Char where
  pronounce c = unwords ["the","character", "'"++[c]++"'"]

instance Pronounceable Integer where
  pronounce n = say n

instance Pronounceable Int where
  pronounce n = say (toInteger n)

instance Pronounceable Double where
  pronounce n = say (truncate n) ++ " point " ++ say (truncate (n * 10) `mod` 10)

instance Pronounceable a => Pronounceable [a] where
  pronounce xs = "a list containing " ++ (intercalate ", " $ map pronounce xs)

instance (Pronounceable a, Pronounceable b) => Pronounceable (a, b) where
  pronounce (a, b) = "a tuple containing " ++ "(" ++ pronounce a ++ ", " ++ pronounce b ++ ")"
