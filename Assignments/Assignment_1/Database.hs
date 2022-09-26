module Database where

type Person = (Name, Age, FavouriteCourse)

type Name             = String
type Age              = Integer
type FavouriteCourse  = String

elena, peter, pol, lucas, tom, richard, frits :: Person
elena   =  ("Elena",  33,  "Functional Programming")
peter   =  ("Peter",  57,  "Imperative Programming")
pol     =  ("Pol",    36,  "Object Oriented Programming")
tom     =  ("Tom",    33,  "Object Oriented Programming")
richard =  ("Richard",    19,  "Imperative Programming")
lucas   =  ("Lucas",    22,  "Engineering Cryptographic Software")
frits   =  ("Frits",    25,  "Functional Programming")

students :: [Person]
students = [elena, peter, pol, tom, richard, lucas, frits]

age :: Person -> Age
age (_, n, _)  =  n

name :: Person -> Name
name (n, _, _) = n

favouriteCourse :: Person -> FavouriteCourse
favouriteCourse (_, _, n) = n

showPerson :: Person -> String
showPerson (name, age, course) = "Name: " ++ name ++ ", Age: " ++ show age ++ ", Favourite Course: " ++ course

twins :: Person -> Person -> Bool
twins (_, age1, _) (_, age2, _) = age1 == age2

increaseAge :: Person -> Person
increaseAge (name, age, course) = (name, age + 1, course)

-- first develop the expressions in GHCi, then replace the TODO's below with them
query1 :: [Person]
query1 = map (increaseAge . increaseAge) students

query2 :: [Person]
query2 = map promote students where promote (name, age, course) = ("dr. " ++ name, age, course)

query3 :: [Person]
query3 = filter frits students where frits (name, _, _) = name == "Frits"

query4 :: [Person]
query4 = filter twenties students where twenties (_, age, _) = age >= 20 && age < 30

query5 :: [Person] -> Age
query5 persons = 
    let totalAge = sum (map age persons) 
        totalElements = toInteger (length persons) 
    in totalAge `div` totalElements

query6 :: [Person]
query6 = map promote students
    where promote student = if favouriteCourse student == "Functional Programming" then ("dr. " ++ name student, age student, favouriteCourse student) else student
