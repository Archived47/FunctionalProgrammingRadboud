module Truthy where

data Nope = Nope
  deriving (Show)

class Truthy a where
  truthy :: a -> Bool

instance Truthy Bool where
  truthy x = x

instance Truthy Int where
  truthy x = x > 0

instance Truthy Nope where
  truthy x = False

instance (Truthy a, Truthy b) => Truthy (a, b) where
  truthy (x, y) = truthy x || truthy y

(&&&) :: (Truthy a, Truthy b) => a -> b -> Bool
x &&& y = truthy x && truthy y
(|||) :: (Truthy a, Truthy b) => a -> b -> Bool
x ||| y = truthy x || truthy y
ifThenElse :: (Truthy a) => a -> b -> b -> b
ifThenElse x y z = if truthy x then y else z

