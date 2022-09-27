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

--member :: (Ord a) => a -> Tree a -> Bool
--insert :: (Ord a) => a -> Tree a -> Tree a
--delete :: (Ord a) => a -> Tree a -> Tree a
--fromList :: (Ord a) => [a] -> Tree a

{----------- exercise 4.5 -------------}

--inOrder :: Tree a -> [a]
--fromAscList :: [a] -> Tree a
--breadthFirst :: Tree a -> [a]

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
