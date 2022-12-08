module RandomGen where

import RandomState
import LCG

import System.Random
import Control.Monad

getRandomRange :: (Int,Int) -> RandomState Int
getRandomRange (a,b) = do
  seed <- getState
  let (randomNumber, newSeed) = randomRange (a, b) seed
  putState newSeed
  return randomNumber

-- multiSim :: [RandomState a] -> RandomState [a]
-- I am guessing this had to be multiEval?
multiSim :: [RandomState a] -> ([a], GlobalState)
multiSim xs = runState (sequence xs) (mkSeed 42)

roll_2d6 :: RandomState Int
roll_2d6 = do
  a <- getRandomRange (1,6)
  b <- getRandomRange (1,6)
  pure (a+b)

runRandomStateIO :: RandomState a -> IO a
runRandomStateIO action = do
  seed <- randomIO
  let (result, newSeed) = runState action (mkSeed seed)
  return result

--these definitions can be used to test your function a bit more thoroughly
runRandomNumbers :: (Int,Int) -> Int -> Seed -> [Int]
runRandomNumbers range n seed = result
  where (result, _) = runState (replicateM n (getRandomRange range)) seed

testme :: [Int]
testme = runRandomNumbers (0,999) 100 (mkSeed 42)