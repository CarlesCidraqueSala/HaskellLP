-- Implement a function myMap :: (a -> b) -> [a] -> [b] that emulates map using comprehension lists.
myMap :: (a -> b) -> [a] -> [b]

myMap _ [] = []
myMap f (x:xs) = [ f y | y <- (x:xs)]

-- Implement a function myFilter :: (a -> Bool) -> [a] -> [a] that emulates filter using comprehension lists.

myFilter :: (a -> Bool) -> [a] -> [a]

myFilter _ [] = []
myFilter ff (x:xs) = [y | y <- (x:xs), ff y == True]

--Implement a function myZipWith :: (a -> b -> c) -> [a] -> [b] -> [c] that emulates zipWith using comprehension lists and zip.

myZipWith :: (a -> b -> c) -> [a] -> [b] -> [c]

myZipWith f lx ly = [f x y | (x,y) <- zip lx ly]

-- Implement a function thingify :: [Int] -> [Int] -> [(Int, Int)] that, given two lists of integers,
-- returns the list that pairs the elements if the element of the second list divides the one in the first list.

thingify :: [Int] -> [Int] -> [(Int, Int)]

thingify lx ly = [(x, y) | x <- lx, y <- ly, rem x y == 0]

--Implement a function factors :: Int -> [Int] that, given a non-null natural number, 
--generates the ordered list with all its factors (non necessaryly primes).

factors :: Int -> [Int]

factors x = [fac | fac <- [1..x], rem x fac == 0]


