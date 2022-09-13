module Char where

import Data.Char

(~~) :: String -> String -> Bool
(~~) a b = map toLower a == map toLower b

reverseCase :: String -> String
reverseCase = map (\c -> if isUpper c then toLower c else toUpper c)

shift :: Int -> Char -> Char
shift n c = if isAsciiUpper c then asciiShift n c else c
    where
        asciiShift :: Int -> Char -> Char
        asciiShift n c
            | ascii + n > ord 'Z' = chr (result - 26)
            | otherwise = chr result
            where
                ascii = ord c
                result = ascii + n

caesar :: Int -> String -> String
caesar n = map (shift n . toUpper)

msg :: String
msg = "ADMNO D HPNO NKMDIFGZ TJP RDOC AVDMT YPNO"
