module Main where

import Data.List
import Data.Char
import Data.Maybe
import System.Random
import System.IO

{---------------------- functional parts -------------------------------}

--data Colour = ...

--scoreAttempt :: (Ord a) => [a] -> [a] -> ???
scoreAttempt code guess = error "implement me"

-- Some test cases from: https://www.boardgamecapital.com/game_rules/mastermind.pdf
test1, test2, test3, test4, test5 :: Bool
test1 = scoreAttempt [1,2,3,4,5 :: Int] [2,6,3,7,8 :: Int] == (1,1)
test2 = scoreAttempt [1,2,3,4,2 :: Int] [5,6,3,3,7 :: Int] == (1,0)
test3 = scoreAttempt [1,2,1,3,3 :: Int] [4,1,5,6,7 :: Int] == (0,1)
test4 = scoreAttempt [4,1,5,6,7 :: Int] [1,2,1,3,3 :: Int] == (0,1)
test5 = scoreAttempt [4,1,5,1,7 :: Int] [1,2,1,3,3 :: Int] == (0,2)

{---------------------- IO parts -------------------------------}

-- only here for example; you can remove these from your final file
roll_d6 :: IO Int
roll_d6 = randomRIO (1,6)

roll_2d6 :: IO Int
roll_2d6 = do
  a <- roll_d6
  b <- roll_d6
  pure (a + b)

--getCode :: ??? -> IO [Colour]

--playGame :: ??? -> IO ()

main :: IO ()
main = do
  putStrLn "IMPLEMENT ME"
