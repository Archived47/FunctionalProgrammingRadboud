module AST where


-- data Expr = Lit Integer | Add Expr Expr
data Expr = Lit Integer | Add Expr Expr | Sub Expr Expr | Mul Expr Expr | Div Expr Expr | VarX
  deriving (Show)

eval :: (Fractional a, Eq a) => Expr -> a -> Maybe a
eval (Lit lit) x = Just (fromInteger lit)
eval VarX x = Just x
eval (Add e1 e2) x = case (eval e1 x, eval e2 x) of
  (Just x, Just y) -> Just (x + y)
  _ -> Nothing
eval (Sub e1 e2) x = case (eval e1 x, eval e2 x) of
  (Just x, Just y) -> Just (x - y)
  _ -> Nothing
eval (Mul e1 e2) x = case (eval e1 x, eval e2 x) of
  (Just x, Just y) -> Just (x * y)
  _ -> Nothing
eval (Div e1 e2) x = case (eval e1 x, eval e2 x) of
  (_, Just 0) -> Nothing
  (Just x, Just y) -> Just (x / y)
  _ -> Nothing