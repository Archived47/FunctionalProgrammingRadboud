module AST where
import Result (Result, Result (Error), Result (Okay))

-- this template uses infix constructors; feel free to use AST2.hs (which uses prefix ones) if you prefer
-- (if you really liked your own solution to Exercise 4.7, you can use that as well)

type Identifier = String

data Expr = Lit Integer | Var Identifier | Expr :+: Expr | Expr :-: Expr | Expr :*: Expr | Expr :/: Expr
  deriving (Show)

infixl 6 :+:
infixl 6 :-:
infixl 7 :/:
infixl 7 :*:

eval :: (Fractional a, Eq a) => Expr -> [(Identifier,a)] -> Result a
eval (Lit k) _ = pure (fromInteger k)
eval (x :+: y) vars = (+) <$> eval x vars <*> eval y vars
eval (x :-: y) vars = (-) <$> eval x vars <*> eval y vars
eval (x :*: y) vars = (*) <$> eval x vars <*> eval y vars
eval (x :/: y) vars = case (eval x vars, eval y vars) of
  (_, Okay 0)  -> Error ["Cannot divide by 0"]
  (Okay x', Okay y') -> pure (x'/y')
  _ -> Error ["Invalid input in divide"]
eval (Var name) vars = case lookup name vars of
  Nothing -> Error ["Variable not found: " ++ name]
  Just a -> Okay a

vars :: (Fractional a, Eq a) => [(String, a)]
vars = [("x",5), ("y",37), ("z", 3)]
expr1 = eval (Var "x" :+: Var "y") vars
expr2 = eval (Var "y" :-: Var "x") vars
expr3 = eval (Var "x" :+: Var "y") []
expr4 = eval (Var "z" :*: Lit 2) vars
expr5 = eval (Var "z" :/: (Lit 2 :-: Lit 2)) vars
