module Tree where

data Tree a = Leaf | Node a (Tree a) (Tree a)
  deriving (Show, Eq)

{----------- exercise 4.3 -------------}

leaves :: Tree a -> Int
leaves Leaf = 1
leaves (Node n branch1 branch2) = leaves branch1 + leaves branch2

nodes :: Tree a -> Int
nodes Leaf = 0
nodes (Node n b c) = 1 + nodes b + nodes c

height :: Tree a -> Int
height Leaf = 0
height (Node n a b) = 1 + max (height a) (height b)

elems :: Tree a -> [a]
elems Leaf = []
elems (Node n a b) = [n] ++ elems a ++ elems b

isSearchTree :: (Ord a) => Tree a -> Bool
isSearchTree Leaf = True
isSearchTree (Node n Leaf Leaf) = True
isSearchTree (Node n (Node a b c) Leaf) = n > a && isSearchTree (Node a b c)
isSearchTree (Node n Leaf (Node a b c)) = n < a && isSearchTree (Node a b c)
isSearchTree (Node n (Node a b c) (Node d e f)) = n > a && isSearchTree (Node a b c) && n < d && isSearchTree (Node d e f)

{----------- exercise 4.4 -------------}

member :: (Ord a) => a -> Tree a -> Bool
member item Leaf = False
member item (Node n a b)
  | item < n = member item a
  | item > n = member item b
  | otherwise = item == n

insert :: (Ord a) => a -> Tree a -> Tree a
insert item Leaf = Node item Leaf Leaf
insert item (Node n a b)
  | item < n = Node n (insert item a) b
  | item > n = Node n a (insert item b)
  | otherwise = Node n a b

delete :: (Ord a) => a -> Tree a -> Tree a
delete item Leaf = Leaf
delete item (Node n Leaf Leaf) = if n == item then Leaf else Node n Leaf Leaf
delete item (Node a Leaf (Node b c d)) = if item == a then Node b c d else Node a (delete item (Node b c d)) Leaf
delete item (Node a (Node b c d) Leaf) = if item == a then Node b c d else Node a (delete item (Node b c d)) Leaf
delete item (Node a (Node b c d) (Node e f g))
  | item < a = Node a (delete item (Node b c d)) (Node e f g)
  | item > a = Node a (Node b c d) (delete item (Node e f g))
  | otherwise = Node b c (insertSmallerTree d (Node e f g))
  where
    insertSmallerTree :: (Ord a) => Tree a -> Tree a -> Tree a
    insertSmallerTree Leaf tree = tree
    insertSmallerTree smaller Leaf = smaller
    insertSmallerTree smaller (Node a Leaf b) = Node a smaller b
    insertSmallerTree smaller (Node a b c) = Node a (insertSmallerTree smaller b) c

fromList :: (Ord a) => [a] -> Tree a
fromList [] = Leaf
fromList [x] = Node x Leaf Leaf
fromList (x:xs) = insert x (fromList xs)

{----------- exercise 4.5 -------------}

inOrder :: Tree a -> [a]
inOrder Leaf = []
inOrder (Node a b c) =  inOrder b ++ [a] ++ inOrder c

fromAscList :: [a] -> Tree a
fromAscList [] = Leaf
fromAscList [x] = Node x Leaf Leaf
fromAscList lst = Node (head sndSplt) (fromAscList (fst splt)) (fromAscList (tail sndSplt))
  where
    splt = splitAt (length lst `div` 2) lst
    sndSplt = snd splt

breadthFirst :: Tree a -> [a]
breadthFirst t = helper [t]
  where 
    helper :: [Tree a] -> [a]
    helper [] = []
    helper (Leaf : xs) = helper xs
    helper ((Node a b c) : xs) = a : helper (xs ++ [b, c])

{- BONUS: a tree pretty printer; the recursive structure of this function
 - is prety simple, but it is a fiddly function to write if you want it to
 - produce an actually nice tree. -}

layout :: (Show a) => Tree a -> String
layout tree = go "" ("","","") tree
  where
  width = maximum (0:[ length (show e) | e <- elems tree ])
  pad s = let s' = show s in replicate (width-length s') '-' ++ s'
  fill  = replicate width ' '

  --go pre (_,_,preN) Leaf = pre ++ preN ++ "·\n" -- this explicitly draws the leaves
  --go _   _          Leaf = ""                   -- this vertically compresses the tree
  go pre _          Leaf = pre ++ "\n"            -- use more vertical space, but don't draw leaves
  go pre (preR,preL,preN) (Node k lt rt)
    = go (pre ++ preR) (hfill,v_bar,rbend) rt
      ++ (pre ++ preN) ++ pad k ++ junct ++
      go (pre ++ preL) (v_bar,hfill,lbend) lt

  junct = "┤\n"         -- change to "+\n" if no Unicode
  hfill = fill ++ "  "
  rbend = fill ++ "/-"  -- change to "/-" if no Unicode
  v_bar = fill ++ "│ "  -- change to "| " if no Unicode
  lbend = fill ++ "\\-"  -- change to "\\-" if no Unicode

putTree :: (Show a) => Tree a -> IO()
putTree tree = putStr (layout tree)
