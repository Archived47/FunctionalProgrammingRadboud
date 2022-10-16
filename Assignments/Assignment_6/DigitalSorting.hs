module DigitalSorting where

import Data.List
import Data.Char
import Deck
import Data.Maybe

class Rankable key where
  rank :: [(key,a)] -> [[a]]

digitalSortOn :: (Rankable key) => (v -> key) -> [v] -> [v]
digitalSortOn f = concat . rank . map (\x->(f x, x))

digitalSort :: (Rankable key) => [key] -> [key]
digitalSort = digitalSortOn id

genericRank :: (Ord key) => [(key,a)] -> [[a]]
genericRank = map (map snd) . groupBy (\(a0, _) (a1, _) -> a0 == a1) . sortOn fst
-- genericRank :: [Card] -> [[Value]]
-- genericRank = map (map value) . groupBy (\x y -> suit x == suit y) . sortOn suit

instance Rankable Int where
  rank = genericRank

instance Rankable Integer where
  rank = genericRank

instance Rankable Char where
  rank = genericRank

instance Rankable Bool where
  rank xs = [[value | (key, value) <- xs, not key], [value | (key, value) <- xs, key]]

instance (Rankable key1, Rankable key2) => Rankable (key1, key2) where
  rank = concatMap rank . rank . map assoc
    where
      assoc :: (Rankable key1, Rankable key2) => ((key1,key2),a) -> (key1,(key2,a))
      assoc ((k1, k2), a) = (k1, (k2, a))

instance (Rankable key) => Rankable (Maybe key) where
  rank [] = []
  rank xs = ([value | (Nothing, value) <- xs]) : rank [(key, value) | (Just key, value) <- xs]

instance (Rankable key) => Rankable [key] where
  rank [] = []
  rank xs = []
  -- uncons -> Turns "hello" into Just ('h',"ello")
  -- This gets sent to the Maybe rank, which uses the Tuple rank

----------------------------------------------------------------------------------------------------
-- some test inputs (it would be reasonably for "rank" and "genericRank" to produce the same output)

testPhrase :: [Char]
testPhrase = "Hello, world!"

boolTest :: [(Bool,Char)]
boolTest = [ (isLetter c, c) | c <- testPhrase ]

maybeTest :: [(Maybe Char,Char)]
maybeTest = [ (if isLetter c then Just c else Nothing, c) | c <- testPhrase ]

tupleTest :: [((Bool,Char),Char)]
tupleTest = [ ((isLetter c, c), c) | c <- testPhrase ]

listTest :: [(String,Char)]
listTest = [ (w, c) | w <- groupBy (\x y->isLetter x==isLetter y) testPhrase, c <- w ]

x y = initial (uncons "Hello")
  where
    f :: Maybe (a, [a]) -> Maybe (a, Maybe (a, [a]))
    f Nothing = Nothing
    f (Just (key, value)) = Just (key, uncons value)
    initial Nothing = Nothing
    initial (Just (key, value)) = Nothing