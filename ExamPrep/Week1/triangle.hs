module Triangle where

triangle :: Integer -> Integer -> String
triangle n shift = triangle' n shift
  where 
    maxStarCount = fromIntegral (n - 1) * 2 + 1
    triangle' 0 shift = ""
    triangle' m shift = triangle' (m - 1) shift ++ emptyString shift ++ stars ++ emptyString 0 ++ "\n"
      where
        starCount = fromIntegral (m - 1) * 2 + 1
        stars = concat (replicate starCount "*")
        emptyString shift = concat (replicate (fromIntegral (shift + ((maxStarCount - (toInteger starCount)) `div` 2))) " ")

christmasTree :: Integer -> String
christmasTree n = tree n
  where
    tree 0 = ""
    tree m = tree (m - 1) ++ triangle m (n - m)