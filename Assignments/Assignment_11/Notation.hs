module Notation where

import Data.Time

siri :: IO ()
siri = do
  putStrLn "What is your name?"
  name <- getLine
  now <- getZonedTime
  putStrLn (name ++ formatTime defaultTimeLocale ", the time is %H:%M" now)

-- mayLookup tries to find the value of the key-value-pair list
mayLookup :: (Eq a) => Maybe a -> [(a, b)] -> Maybe b
mayLookup maybekey assocs =
  maybekey >>= \key ->
  lookup key assocs >>= \result ->
  return result
