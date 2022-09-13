module Swap where

swap :: (a, b) -> (b, a)
swap (x,y) = (y,x)

apply :: (Int, Int) -> (Int, Int)
apply (x,y) = (x + y, y)

fibStep :: (Int, Int) -> (Int, Int)
fibStep (x,y) = (y,y + x)