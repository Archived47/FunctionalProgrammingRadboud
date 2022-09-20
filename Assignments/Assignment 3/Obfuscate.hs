module Obfuscate where

import Data.Char (isSpace)

checkpunctuation :: Char -> Bool
checkpunctuation c = c `elem` ['.', ',', '?', '!', ':', ';', '(', ')']

words' :: String -> [String]
words' s = case dropWhile isSpace' s of
  "" -> []
  s' -> if checkpunctuation (head s'') then (w ++ [head s'']) : words' s'' else w : words' (tail s'')
    where
      (w, s'') = break isSpace' s'
  where
    isSpace' s = isSpace s || checkpunctuation s

getRandomNumbers :: Int -> Int
getRandomNumbers n = a * a * a `mod` m
  where
    a = n * 15485863
    m = 2038074743

shuffle :: [Int] -> [a] -> [a]
shuffle [] [] = []
shuffle [] (_:_) = []
shuffle (_:_) [] = []
shuffle (i:is) xs = let (firsts, rest) = splitAt (i `mod` length xs) xs
                     in head rest : shuffle is (firsts ++ tail rest)

shuffleMiddle :: [a] -> [a]
shuffleMiddle str = if length str > 1 then head str : shuffle randomNumbers ((init . tail) str) ++ [last str] else str
  where
    randomNumbers = [getRandomNumbers j | j <- [1..length str - 2]]


cambridge :: String -> String
cambridge str = unwords [shuffleMiddle word | word <- allWords]
  where
    allWords = words' str

meme :: String
meme = "According to research at Cambridge university, it doesn't matter\
       \ what order the letters in a word are, the only important thing is\
       \ that the first and last letters are at the right place. The rest can\
       \ be a total mess and you can still read it without a problem. This is\
       \ because we do not read every letter by it self but the word as a wohle."