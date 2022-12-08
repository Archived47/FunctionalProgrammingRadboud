module Dice where

import System.Random
import Control.Monad
import Control.Applicative
import Data.List
import RandomState
import RandomGen
import LCG
import Exception (try, SomeException (SomeException), evaluate)

data Expr =
    Lit Int
    | Dice Int
    | Expr :+: Expr
    | Expr :-: Expr
    | Expr :/: Int
    | Min Expr Expr
    | Max Expr Expr
  deriving (Show)

infixl 6 :+:

type DiceAction m = Int -> m Int

-- evalM :: Expr -> DiceAction IO -> IO Int             -- prototype
-- evalM (Lit a) action = return a
-- evalM (Dice a) action = action a
-- evalM (x :+: y) action = (+) <$> evalM x action <*> evalM y action
-- evalM (Min x y) action = min <$> evalM x action <*> evalM y action
-- evalM (Max x y) action = min <$> evalM x action <*> evalM y action

evalM :: (Monad m) => Expr -> DiceAction m -> m Int  -- final version
evalM (Lit a) action = return a
evalM (Dice a) action = action a
evalM (e1 :/: a) action = do {result <- evalM e1 action; return (result `div` a); }
evalM (e1 :+: e2) action = (+) <$> evalM e1 action <*> evalM e2 action
evalM (e1 :-: e2) action = (-) <$> evalM e1 action <*> evalM e2 action
evalM (Min e1 e2) action = min <$> evalM e1 action <*> evalM e2 action
evalM (Max e1 e2) action = min <$> evalM e1 action <*> evalM e2 action

evalRIO :: Expr -> IO Int
-- evalRIO expr = evalM expr (\dice->randomRIO (1,dice) >>= return) -- silent version
evalRIO expr = evalM expr (\dice->randomRIO (1,dice) >>= report) -- verbose version
 where report x = do { putStr "rolled a "; print x; return x }

evalIO :: Expr -> IO Int
evalIO expr = do {
    result <- evalM expr (\dice->(action dice) >>= return);
    putStr "You rolled a total of ";
    return result;
  }
  where
    action :: Int -> IO Int
    action dice = do
      putStr ("Please give an integer between 1 and " ++ show dice ++ ": ")
      input <- getLine
      parsedInputTry <- try (evaluate (read input)) :: IO (Either SomeException Int)
      case parsedInputTry of
        Left se -> do
          putStrLn (show input ++ " is not in range: 1-" ++ show dice)
          action dice
        Right parsedInput -> do
          if parsedInput > dice || parsedInput < 0 then do
            putStrLn (show parsedInput ++ " is not in range: 1-" ++ show dice)
            action dice
          else
            return parsedInput

evalND :: Expr -> [Int]
evalND expr = evalM expr (\dice -> [1..dice])

avg :: (Fractional a) => [Int] -> a
avg xs = fromIntegral (sum xs) / fromIntegral (length xs)

expectation :: (Fractional a) => Expr -> a
expectation e = avg (evalND e)

evalR :: Expr -> RandomState Int
evalR expr = evalM expr (\dice -> getRandomRange (1,dice))

observed :: (Fractional a) => Int -> Expr -> RandomState a
observed n expr = avg <$> replicateM n (evalR expr)
