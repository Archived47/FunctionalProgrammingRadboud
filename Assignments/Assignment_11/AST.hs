module AST where
import Result (Result)

-- this template uses infix constructors; feel free to use AST2.hs (which uses prefix ones) if you prefer
-- (if you really liked your own solution to Exercise 4.7, you can use that as well)

type Identifier = String

data Expr = Lit Integer | Var Identifier | Expr :+: Expr | Expr :-: Expr | Expr :*: Expr | Expr :/: Expr
  deriving (Show)

infixl 6 :+:
infixl 6 :-:
infixl 7 :/:
infixl 7 :*:

eval :: (Fractional a, Eq a) => Expr -> [(Identifier,a)] -> Maybe a
eval (Lit k) _ = Just (fromInteger k)
eval (x :+: y) vars = (+) <$> eval x vars <*> eval y vars
eval (x :-: y) vars = (-) <$> eval x vars <*> eval y vars
eval (x :*: y) vars = (*) <$> eval x vars <*> eval y vars
eval (x :/: y) vars = case (eval x vars, eval y vars) of
  (_, Just 0)  -> Nothing
  (Just x', Just y') -> Just (x'/y')
  _ -> Nothing

eval (Var name) vars = lookup name vars

vars :: (Fractional a, Eq a) => [(String, a)]
vars = [("x",5), ("y",37), ("z", 3)]
expr1 = eval (Var "x" :+: Var "y") vars
expr2 = eval (Var "y" :-: Var "x") vars
expr3 = eval (Var "x" :+: Var "y") []
expr4 = eval (Var "z" :*: Lit 2) vars
expr5 = eval (Var "z" :/: ((Lit 2) :-: (Lit 2))) vars
