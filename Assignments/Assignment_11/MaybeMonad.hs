module MaybeMonad where

-- maybeMap :: (a -> b) -> Maybe a -> Maybe b
-- maybeMap f (Just a) = Just (f a)
-- maybeMap _ _ = Nothing

-- stripMaybe :: Maybe (Maybe a) -> Maybe a
-- stripMaybe (Just x) = x
-- stripMaybe _ = Nothing

-- applyMaybe:: (a -> Maybe b) -> Maybe a -> Maybe b
-- applyMaybe f (Just x) = f x
-- applyMaybe _ _ = Nothing

maybeMap :: (a -> b) -> Maybe a -> Maybe b
maybeMap = fmap

stripMaybe :: Maybe (Maybe a) -> Maybe a
stripMaybe x  = x >>= id

applyMaybe:: (a -> Maybe b) -> Maybe a -> Maybe b
applyMaybe f x = f =<< x


-- Monad check

monadMap :: (Monad m) => (a -> b) -> m a -> m b
monadMap = fmap

stripMonad :: (Monad m) => m (m a) -> m a
stripMonad x  = x >>= id

applyMonad:: (Monad m) => (a -> m b) -> m a -> m b
applyMonad f x = f =<< x
