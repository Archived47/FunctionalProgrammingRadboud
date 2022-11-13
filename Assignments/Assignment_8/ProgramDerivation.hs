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
Derive
  inorderCat t xs = inorder t ++ xs

  Case 1: t = Leaf
  inorderCat Leaf xs
  = inorder Leaf ++ xs
  = [] ++ xs
  = xs

  Case 2: t = Node x l r
  IH: inorderCat t xs = inorder t ++ xs
  inorderCat (Node x l r) xs
  = { inorderCat specification }
  inorder (Node x l r) ++ xs
  = { inorder definition }
  (inorder l ++ [x] ++ inorder r) ++ xs 
  = { rewrite }
  (inorder l ++ [x]) ++ (inorder r ++ xs)
  = { IH }
  (inorder l ++ [x]) ++ (inorderCat r xs)
  = { [x] ++ => x : }
  inorder l ++ (x : inorderCat r xs)
  = { IH }
  inorderCat l (x : (inorderCat r xs))
-}

inorderCat :: Tree a -> [a] -> [a]
inorderCat Leaf xs = xs
inorderCat (Node x l r) xs = inorderCat l (x : inorderCat r xs)

inorder' :: Tree a -> [a]
inorder' t = inorderCat t []

-- 2. Yes, it is more efficient, with "inorder' (skewed 10000)" it generates a lot faster than with "inorder (skewed 10000)" 

-- TODO: make me more efficient, too
{-

Derive
  elems (Node x lt rt) = x : elems lt ++ elems rt
  inorderCat' t xs = elems t ++ xs

  Case 1: t = Leaf
  inorderCat' Leaf xs
  = elems Leaf ++ xs
  = [] ++ xs
  = xs

  Case 2: t = Node x lt rt
  IH: inorderCat' t xs = elems t ++ xs
  inorderCat' (Node x lt rt) xs
  = { inorderCat' specification }
  elems (Node x lt rt) ++ xs
  = { elems definition }
  x : elems lt ++ elems rt ++ xs
  = { IH }
  x : elems lt ++ (inorderCat' rt xs)
  = { IH }
  x : inorderCat' lt (inorderCat' rt xs)
-}

-- TODO: make me more efficient, too
elems :: Tree a -> [a]
elems Leaf = []
elems (Node x lt rt) = x : inorderCat lt (inorderCat rt [])

