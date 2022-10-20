module FindDefs where
import Data.Maybe (isJust, fromMaybe)

mapFilter :: (a -> Maybe b) -> [a] -> [b]
mapFilter f = map $ (\(Just x) -> x) . f

data Pair a = Pair a a deriving Show
instance Functor Pair where
    fmap f (Pair x y) = Pair (f x) (f y)

lift :: (a -> b -> Maybe c) -> (Maybe a -> Maybe b -> Maybe c)
lift f (Just x1) (Just y1) = f x1 y1

compute :: (Monoid n) => (a -> n) -> [a] -> n
compute f = mconcat . map f

fuse :: (a -> b -> c) -> (a -> b) -> a -> c
fuse fa fb x = fa x (fb x)
