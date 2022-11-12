> module TreeInduction where
> 
> data Tree a = Leaf | Node a (Tree a) (Tree a)
>   deriving (Show)

1: What is the induction scheme for trees?
The base case:
Show that the property holds for P(Leaf)

The induction case:
P(tree) => for all l and r, P(Node x tree)

Local definitions:

> leaves :: Tree a -> Int
> leaves Leaf = 1
> leaves (Node _ l r) = leaves l + leaves r
>
> nodes :: Tree a -> Int
> nodes Leaf = 0
> nodes (Node _ l r) = 1 + nodes l + nodes r

2: To prove: leaves t = nodes t + 1
By induction on t.

Case t = Leaf

    leaves Leaf
    ----------------- definition of leaves
  = 1
  --                  definition of nodes
  = 0 + 1
  --                  specification of nodes
  = nodes Leaf + 1

Case t = Node x l r
IH1: leaves l = nodes l + 1
IH1: leaves r = nodes r + 1

    leaves (Node x (l) (r))
    --------------------------------------- ???
  = leaves l + leaves r
  --                                        IH
  = (nodes l + 1) + (nodes r + 1)
  --                                        rewrite
  = nodes l + nodes r + 1 + 1
  --                                        definition of nodes
  = (1 + nodes l + nodes r) + 1
  --                                        specification of nodes
  = nodes (Node x (l) (r)) + 1