{-# LANGUAGE InstanceSigs #-}

module Result where

import Data.List

data Result a = Okay a | Error [String]
  deriving (Eq, Ord, Show)

instance Functor Result where
  fmap :: (a -> b) -> Result a -> Result b
  fmap _ (Error a) = Error a
  fmap f (Okay a) = Okay (f a)

expr1 = fmap reverse (Okay [1, 2, 3])
expr2 = fmap reverse (Error ["list is empty", "not divisible by 5"])

instance Applicative Result where
  pure :: a -> Result a 
  pure r = Okay r

  (<*>) :: Result (a -> b) -> Result a -> Result b
  rf <*> (Error ra) = case rf of
    Okay fab -> Error ra
    Error ss -> Error (ss ++ ra)
  rf <*> ra = case rf of
    Okay fab -> fmap fab ra
    Error ss -> Error ss

expr3 = (*) <$> Okay 6 <*> Okay 7
expr4 = (++) <$> Okay [1,2,3] <*> Okay [4,5,6]
expr5 = (++) <$> Okay [1,2,3] <*> Error ["invalid arguments"]
expr6 = (*) <$> Error ["division by zero"] <*> Error ["not a number","unknown variable: x"]