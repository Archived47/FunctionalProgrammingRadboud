module Replicate where

-- this is the definition in the slides:
replicateM' :: (Monad m) => Int -> m a -> m [a]
replicateM' 0 _  = return []
replicateM' n mx = (:) <$> mx <*> replicateM' (n-1) mx

replicateM :: (Monad m) => Int -> m a -> m [a]
replicateM 0 _  = return []
replicateM n mx = do 
  f <- fmap (:) mx
  result <- replicateM (n - 1) mx
  return (f result)

-- IO
-- Asks for the getLine function 4 times
e1 = replicateM 4 getLine
-- Maybe
-- Just prints out Nothing again
e2 = replicateM 4 Nothing
-- Maybe
-- Makes a Maybe list of 37s
e3 = replicateM 4 (Just 37)
-- Integer
-- Makes a list of lists of size 4 that will have all combinations between 0 and 1
e4 = replicateM 4 [0,1]