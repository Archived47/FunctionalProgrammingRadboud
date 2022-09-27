module ADTTypes where

import ADTs

isWeekend :: Day -> Bool
isWeekend Sat = True
isWeekend Sun = True
isWeekend _ = False

bval :: Prop -> Bool
bval prop = case prop of
  T -> True
  F -> False
  (p :-> q) -> not (bval p) || bval q

incr :: Foo [String] -> Foo String
incr (Bar k) = Bar (unlines k)

wrap :: (Eq t, Num t) => t -> Wrapped a -> Wrapped (Wrapped a)
wrap 0 x = Bare x
wrap n x = wrap (n -1) (Wrapped x)
