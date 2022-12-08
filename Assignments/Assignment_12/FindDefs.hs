module FindDefs where

import System.Random (randomIO)

sayIO :: IO Int -> IO String
sayIO = fmap show

sayMaybe :: Maybe Int -> Maybe String
sayMaybe = fmap show

mpair :: (Monad m) => m a -> m b -> m (a, b)
mpair a b = do {a1 <- a; b1 <- b; return (a1, b1)}

-- weirdBind :: (Monad m) => Maybe (m a) -> (a -> m b) -> m (Maybe b)
-- weirdBind mybe f = mybe >>= 5
  

ex1 = sayIO randomIO
ex2 = sayMaybe (Just 5)
ex3 = mpair (Just 1) (Just 2)
