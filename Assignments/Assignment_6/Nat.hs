module Nat where

data Nat = O | S Nat
  deriving (Show)

fromNat :: (Num t) => Nat -> t
fromNat O     = 0
fromNat (S x) = fromNat x + 1

toNat :: (Ord t, Num t) => t -> Nat
toNat 0 = O
toNat n = S (toNat (n - 1))

instance Eq Nat where 
  O == O = True
  O == (S x) = False
  (S x) == O = False
  (S x) == (S y) = x == y

instance Ord Nat where 
  O <= O = True
  O <= (S x) = True
  (S x) <= O = False
  (S x) <= (S y) = x <= y

instance Enum Nat where
  succ x = S x
  pred O = O
  pred (S x) = x
  toEnum x = toNat x
  fromEnum x = fromNat x
