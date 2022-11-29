module FindDefs where

-- (?$) :: Maybe (a -> b) -> Maybe a -> Maybe b
-- f ?$ a = case f of
--   Nothing -> Nothing
--   Just fab -> case a of
--     Nothing -> Nothing
--     Just a' -> Just (fab a')

(?$) :: Maybe (a -> b) -> Maybe a -> Maybe b
Just f ?$ Just a = Just (f a)
_ ?$ _ = Nothing

pair :: (Applicative f) => f a -> f b -> f (a,b)
pair a b = (,) <$> a <*> b

apply :: [a -> b] -> a -> [b]
apply fs a = map ($ a) fs

apply2nd :: [a -> b -> c] -> b -> [a -> c]
apply2nd fs x = fmap (`flip` x) fs
