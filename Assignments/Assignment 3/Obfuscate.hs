module Obfuscate where

import Data.Char ( isPunctuation )
import Data.List ( sortBy )

getRandomNumber :: Int -> Int
getRandomNumber n = a * a * a `mod` m
  where
    a = n * 15485863
    m = 2038074743

shuffle :: [Int] -> [a] -> [a]
shuffle ns xs = map fst (sortBy (\(_,a) (_,b) -> compare a b) [(x, i) | (x, i) <- zip xs ns])

shuffleMiddle :: String -> String
shuffleMiddle [c] = [c]
shuffleMiddle str = head str : shuffle randomNumbers middle ++ end
  where
    randomNumbers = [getRandomNumber j | j <- [1..length str]]
    begin = init str
    end = if isPunctuation (last str) then reverse [last str, last begin] else [last str]
    middle = if isPunctuation (last str) then (init . tail) begin else tail begin

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