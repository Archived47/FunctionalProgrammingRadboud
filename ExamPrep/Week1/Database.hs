module Database where

type Person = (Name, Age, FavouriteCourse)

type Name             = String
type Age              = Integer
type FavouriteCourse  = String

elena, peter, pol, lucas :: Person
elena  =  ("Elena",  33,  "Functional Programming")
peter  =  ("Peter",  57,  "Imperative Programming")
pol    =  ("Pol",    36,  "Object Oriented Programming")
frits    =  ("Frits",    36,  "Object Oriented Programming")
lucas  =  ("Lucas",    22,  "Functional Programming")

students :: [Person]
students = [elena, peter, pol, lucas, frits]

age :: Person -> Age
age (_, n, _)  =  n

name :: Person -> Name
name (n, _, _) = n

favouriteCourse  :: Person -> FavouriteCourse
favouriteCourse (_, _, n) = n

showPerson :: Person -> String
showPerson person = "Name: " ++ name person ++ ", Age: " ++ show (age person) ++ ", Favourite Course: " ++ favouriteCourse person

twins :: Person -> Person -> Bool
twins pa pb = age pa == age pb

increaseAge :: Person -> Person
increaseAge (n, a, c) = (n, a + 1, c)

query1 :: [Person]
-- query1 = [(n, a + 2, c) | (n, a, c) <- students]
query1 = map (increaseAge . increaseAge) students -- Better way because we are reusing an already existing function

query2 :: [Person]
query2 = map (\(n, a, c) -> ("dr. " ++ n, a, c)) students

query3 :: [Person]
query3 = filter (\x -> name x == "Frits") students

query4 :: [Person]
query4 = filter (\x -> let a = age x in a >= 20 && a < 30) students

query5 :: [Person] -> Age
query5 persons = sum (map age students) `div` (toInteger (length students))

query6 :: [Person]
query6 = map (\(n, a, c) -> if c == "Functional Programming" then ("dr. " ++ n, a, c) else (n, a, c)) students
