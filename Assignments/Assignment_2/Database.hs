module Database where

data Person = Person {name :: String, age :: Integer, favouriteCourse :: String}

type Name = String
type Age = Integer
type FavouriteCourse = String

elena, peter, pol, frits :: Person
elena = Person {name = "Elena", age = 33, favouriteCourse = "Functional Programming"}
peter = Person {name = "Peter", age = 57, favouriteCourse = "Imperative Programming"}
pol   = Person {name = "Pol", age = 36, favouriteCourse = "Object Oriented Programming"}
frits = Person {name = "Frits", age = 25, favouriteCourse = "Functional Programming"}

students :: [Person]
students = [elena, peter, pol, frits]

showPerson :: Person -> String
showPerson person = "Name: " ++ name person ++ ", Age: " ++ show (age person) ++ ", Favourite Course: " ++ favouriteCourse person

twins :: Person -> Person -> Bool
twins a b = age a == age b

increaseAge :: Person -> Person
increaseAge person = person {age = age person + 1}

query1 :: [Person]
query1 = map (increaseAge . increaseAge) students

query2 :: [Person]
query2 = map (\p -> p {name = "dr. " ++ name p}) students

query3 :: [Person]
query3 = filter (\p -> name p == "Frits") students

query4 :: [Person]
query4 = filter (\p -> age p >= 20 && age p < 30) students

query5 :: [Person] -> Age
query5 persons = sum (map age persons) `div` toInteger (length persons)

query6 :: [Person]
query6 = map (\p -> if favouriteCourse p == "Functional Programming" then p {name = "dr. " ++ name p} else p) students
