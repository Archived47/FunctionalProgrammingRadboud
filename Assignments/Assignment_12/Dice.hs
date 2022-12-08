module Dice where

import System.Random
import Control.Monad
import Control.Applicative
import Data.List
import RandomState
import RandomGen
import LCG

data Expr = Lit Int | Dice Int 
          | Expr :+: Expr
          | Min Expr Expr | Max Expr Expr
  deriving (Show)

infixl 6 :+: 

type DiceAction m = Int -> m Int

--evalM :: Expr -> DiceAction IO -> IO Int             -- prototype
--evalM :: (Monad m) => Expr -> DiceAction m -> m Int  -- final version

--evalRIO :: Expr -> IO Int
--evalRIO expr = evalM expr (\dice->randomRIO (1,dice) >>= return) -- silent version
--evalRIO expr = evalM expr (\dice->randomRIO (1,dice) >>= report) -- verbose version
--  where report x = do { putStr "rolled a "; print x; return x }

--evalIO :: Expr -> IO Int

--evalND :: Expr -> [Int]

avg :: (Fractional a) => [Int] -> a
avg xs = fromIntegral (sum xs) / fromIntegral (length xs)

--expectation :: (Fractional a) => Expr -> a
--expectation e = avg (evalND e)

--evalR :: Expr -> RandomState Int

--observed :: (Fractional a) => Int -> Expr -> IO a
