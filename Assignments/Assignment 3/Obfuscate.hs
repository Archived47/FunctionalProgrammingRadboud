module Obfuscate where

import Data.Char ( isPunctuation, isDigit )
import Data.List ( sortBy )

getRandomNumber :: Int -> Int
getRandomNumber n = a * a * a `mod` m
  where
    a = n * 15485863
    m = 2038074743

shuffle :: [Int] -> [a] -> [a]
shuffle ns xs = map fst (sortBy (\(_,a) (_,b) -> compare a b) [(x, i) | (x, i) <- zip xs ns])

shuffleMiddle :: String -> String
shuffleMiddle [] = []
shuffleMiddle [c] = [c]
shuffleMiddle str = head str : shuffle randomizedIndexes middle ++ end
  where
    indexes = [1..length middle]
    filled = [if isPunctuation c || isDigit c then i else 0 | (c , i) <- zip middle [1..]]
    filteredIndexes = filter (== 0) filled
    randomNumbers = [getRandomNumber i | i <- take (length filteredIndexes) indexes]
    randomizedFilteredIndexes = shuffle randomNumbers [j | (i,j) <- zip filled indexes, i == 0]
    randomizedIndexes = fill filled randomizedFilteredIndexes
    begin = init str
    middle = if isPunctuation (last str) then (init . tail) begin else tail begin
    end = if isPunctuation (last str) then reverse [last str, last begin] else [last str]

fill :: (Eq a, Num a) => [a] -> [a] -> [a]
fill [] [] = []
fill xs [] = xs
fill [] ys = ys
fill (x:xs) (y:ys) = if x == 0 then y : fill xs ys else x : fill xs (y : ys)

cambridge :: String -> String
cambridge str = unwords [shuffleMiddle word | word <- words str]

meme :: String
meme = "According to research at Cambridge university, it doesn't matter\
       \ what order the letters in a word are, the only important thing is\
       \ that the first and last letters are at the right place. The rest can\
       \ be a total mess and you can still read it without a problem. This is\
       \ because we do not read every letter by it self but the word as a wohle."

main :: String
main = cambridge meme