{-This problem explores the definition of high-order functions on lists. Implement the followinf functions that work as the original Haskell functions 
without using the original function eachself (i.e., you cannot use foldl ti implement myFoldl but you can use it to implement myAll). 
Additionally. you can only use recursion to implement myFoldl, myFoldr, myIterate, myUntil and myZip. -}

--1)
myFoldl :: (a -> b -> a) -> a -> [b] -> a

myFoldl _ xini [] = xini
myFoldl f xini (y:ys) = myFoldl f (f xini y) ys

--2)
myFoldr :: (a -> b -> b) -> b -> [a] -> b

myFoldr _ b [] = b
myFoldr f b (a:as) = f a (myFoldr f b as)

--3)
myIterate :: (a -> a) -> a -> [a]

myIterate f a = [a] ++ myIterate f (f a)

--4)
myUntil :: (a -> Bool) -> (a -> a) -> a -> a

myUntil b f a  
    | (b a) == True = a
    |otherwise =  myUntil b f (f a)

--5)
myMap :: (a -> b) -> [a] -> [b]

myMap _ [] = []
myMap f (y:ys) = f y : myMap f ys

--6)
myFilter :: (a -> Bool) -> [a] -> [a]

myFilter b ys
    |null ys == True = []
    |b (head ys) == True = head ys : myFilter b (tail ys)
    |otherwise = myFilter b (tail ys)

--7)
myAll :: (a -> Bool) -> [a] -> Bool

myAll b ys = and (map b ys)
    {-
    myAll b ys
    |null ys == True = False
    |(length ys == 1) && (b (head ys) == True) = True
    |b (head ys) == False = False
    |otherwise = myAll b (tail ys)-}

--8)
myAny :: (a -> Bool) -> [a] -> Bool

myAny b ys
    |null ys == True = False
    |b (head ys) == True = True
    |otherwise = myAny b (tail ys)

--9)
myZip :: [a] -> [b] -> [(a, b)]

myZip xs ys
    |null xs == True || null ys == True = []
    |otherwise = ((head xs) , (head ys)) : myZip (tail xs) (tail ys)

--10)
myZipWith :: (a -> b -> c) -> [a] -> [b] -> [c]

myZipWith f xs ys
    |null xs == True || null ys == True = []
    |otherwise = f (head xs) (head ys) : myZipWith f (tail xs) (tail ys)
