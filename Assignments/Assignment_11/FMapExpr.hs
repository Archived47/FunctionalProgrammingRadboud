module FMapExpr where

import Data.Char

-- Adds a 1 to every number in the list
expr1 :: [Integer]
expr1 = fmap (\x->x+1) [1,2,3]

-- Adds "dr." to the maybe Shaak
expr2 :: Maybe [Char]
expr2 = fmap ("dr." ++) (Just "Sjaak")

-- Makes the name lowercase
expr3 :: [Char]
expr3 = fmap toLower "Marc Schoolderman"

-- Adds "dr." to the names in the list if they are not Nothing
expr4 :: [Maybe [Char]]
expr4 = fmap (fmap ("dr." ++)) [Nothing, Just "Marc", Just "Twan"]
