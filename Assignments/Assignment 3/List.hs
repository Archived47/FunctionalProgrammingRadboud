module List where

-- import Data.List

-- # 1
-- xs0 = [1,2,3,4,5]
-- xs1 = [[1,2,3],[4,5]]
-- xs2 = ['a', 'b', 'c']
-- xs3 = []
-- xs4 = [[],[]]
-- xs5 = [[[]]]
-- xs6 = [[]]
-- xs7 = [[[]]]

-- 

-- # 2
xs0 = 1 : 2 : 3 : [] ++ 4 : 5 : []
xs1 = (1 : 2 : 3 : []) : (4 : 5 : []) : []
xs2 = 'a' : 'b' : 'c' : []
xs3 = []
xs4 = [] : [] : []
xs5 = ([] : []) : []
xs6 = [] : []
xs7 = ([] : []) : []

-- # 3
-- Because it can be any Num type, not necessarily an Integer.