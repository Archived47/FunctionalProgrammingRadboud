module Main where

import Data.List (nub, sort, group, intercalate)
import Data.Char ()
import Data.Maybe ()
import System.Random ( randomRIO )
import System.IO ()
import Data.Array ( array, (!) )
import Control.Monad (replicateM)

{---------------------- functional parts -------------------------------}

data Colour = White | Silver | Green | Red | Orange | Pink | Yellow | Blue
  deriving (Ord, Eq, Show)

coloursList :: [Colour]
coloursList = [White, Silver, Green, Red, Orange, Pink, Yellow, Blue]


scoreAttempt :: (Ord a) => [a] -> [a] -> (Int, Int)
scoreAttempt code guess = (score, correctColours - score)
  where
    codeArray = array (1, length code) (zip [1..] code)
    guessArray = array (1, length guess) (zip [1..] guess)

    score = sum [
        if codeArray ! i == guessArray ! i then 1 else 0
        | i <- [1..length codeArray]
      ]

    quantities = map (\x -> (head x, length x)) $ group $ sort guess
    correctColours = sum [min (find (fst g) code) (snd g) | g <- quantities]

    find _ [] = 0
    find n (x:xs)
      | x == n = 1 + find n xs
      | otherwise = find n xs

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

getCode :: Int -> IO [Colour]
getCode length = replicateM length getCode'
  where
    colours = zip [1..8] coloursList
    getCode' = do
      number <- randomRIO (1, 8 :: Int)
      let colour = snd $ head $ filter (\x -> fst x == number) colours
      pure colour

playGame :: [Colour] -> Int -> IO ()
playGame code attempts = do
  -- putStr "Try to guess the secret code word, 12 tries left. \n"
  putStrLn ("Try to guess the secret code word, " ++ show attempts ++ " tries left.")
  putStr "> "
  guess <- getLine

  let parsedColours = parseColours guess
  let result = scoreAttempt code parsedColours
  let isCorrect = fst result == length code

  if isCorrect then do
    putStrLn "Correct"
    return ()
    else do
      putStrLn "Incorrect"
      putStrLn (show (fst result) ++ " colour(s) in the correct position,")
      putStrLn (show (snd result) ++ " colour(s) in the wrong position.")
      if attempts > 1 then
        playGame code (attempts - 1)
        else do
          putStrLn "No more tries, game over."
          putStrLn ("The code was " ++ unwords (map show code))

parseColours :: String -> [Colour]
parseColours str = [parseColour colour | colour <- words str]
  where
    parseColour :: String -> Colour
    parseColour colourString = if null result then error "Wrong input" else head result
      where result = filter (\x -> head (show x) == head colourString) coloursList



main :: IO ()
main = do
  let colours = 4

  putStrLn ("I picked a random code word with " ++ show colours ++" colours.")
  putStrLn ("Possible colours are " ++ unwords (map show coloursList) ++ ".")

  code <- getCode colours
  playGame code 12
