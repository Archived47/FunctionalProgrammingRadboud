import Data.List
import Data.Char
import Data.Function
import qualified Data.Map as M
--  ^^comment out this line if it causes compile errors (we need the `containers` library)

wordFrequency :: String -> [(String,Int)]
wordFrequency = map (\x->(head x,length x)) . group . sort . words

mostFrequentOfLength :: Int -> String -> [String]
mostFrequentOfLength n = map fst . sortBy (\(a0, b0) (a1, b1) -> compare b1 b0) . map (\x->(head x,length x)) . group . sort . filter (\x -> length x >= n) . words

wordLengthFrequency :: String -> [(Int,Int)]
wordLengthFrequency = sortBy (\(a0, b0) (a1, b1) -> compare a0 a1) . map (\x->(head x,length x)) . group . map length . sort . words

anagrams :: String -> [[String]]
anagrams = map (map fst) . filter (\x -> length x > 1) . groupBy (\(a0, b0) (a1, b1) -> b0 == b1) . sortBy (\(a0, b0) (a1, b1) -> compare b0 b1) . map (\x -> (x, sort x)) . dup . sort . words
  where
    dup :: [String] -> [String]
    dup [] = []
    dup (x:xs) = let ys = dup xs in case ys of
      (y:ys') | x == y -> ys'
      _ -> x : ys

{- this 'main' function is only here for the compiled, stand-alone version
 - calling it from within GHCi is problematic since GHCi itself needs stdin!
 - to compile, run:
 -
 -     ghc -O WordStats
 -
 - (The -O flag turns on optimizations)
 -}

main :: IO ()
main = onStdin $ wordFrequency  -- change this to run a different function from the commandline
  where onStdin f = getContents >>= (mapM_ print . f . filter (\x->isAlphaNum x || isSpace x))
