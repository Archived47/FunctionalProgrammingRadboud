module Expression where

data Tree a = Leaf | Node a (Tree a) (Tree a)
  deriving (Show)

skewed :: Integer -> Tree ()
skewed 0 = Leaf
skewed n = Node () (skewed (n-1)) Leaf

inorder :: Tree a -> [a]
inorder Leaf = []
inorder (Node x lt rt) = inorder lt ++ [x] ++ inorder rt

{-
TODO, NO CLUE
Derive
  inorderCat t xs = inorder t ++ xs

  Case 1: t = Leaf
  inorderCat Leaf xs
  = inorder Leaf ++ xs
  = [] ++ xs
  = xs

  Case 2: t = Node x l r
  IH1: inorderCat l xs = inorder l ++ xs
  IH2: inorderCat r xs = inorder r ++ xs
  inorderCat t xs
  = inorder (Node x l r) ++ xs
  = (inorder l ++ [x] ++ inorder r) ++ xs 
  = (inorder l ++ [x]) ++ (inorder r ++ xs)
  = (inorder l ++ [x]) ++ (inorderCat r xs)
-}

inorderCat :: Tree a -> [a] -> [a]
inorderCat t xs = inorder t ++ xs -- TODO: make me more efficient

inorder' :: Tree a -> [a]
inorder' t = inorderCat t []

-- TODO: make me more efficient, too
{-

Derive
  elems (Node x lt rt) = x : elems lt ++ elems rt

  Case 1: t = Leaf
  elems Leaf
  = []

  Case 2: t = Node x lt rt
  elems Node x lt rt
  = x : elems lt ++ elems rt
  = 
  

-}
elems :: Tree a -> [a]
elems Leaf = []
elems (Node x lt rt) = x : elems lt ++ elems rt

