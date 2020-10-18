countIf :: (Int -> Bool) -> [Int] -> Int
--that, given a predicate on integers and a list of integers, returns the number of elements in the list that satify the predicate.
countIf b list
    |null list == True = 0
    |b (head list) == True = 1 + countIf b (tail list)
    |otherwise = countIf b (tail list)
--countIf f a = length (filter f a)

pam :: [Int] -> [Int -> Int] -> [[Int]]
{-that, given a list of integers and a list of functions from integers to integers, 
returns the list consisting if applying each of the functions in the second list to the elements in the first list.-}

pam xs fs = [ map f xs | f <- fs ]

pam2 :: [Int] -> [Int -> Int] -> [[Int]]
{-that, given a list of integers and a list of functions from integers to integers, returns the list of lists where each list if the result of applying,
     one after the other, the function in the second list to each element in the first list. -}
    
pam2 xs fs = [ map (flip ($) x) fs | x <- xs ]

filterFoldl :: (Int -> Bool) -> (Int -> Int -> Int) -> Int -> [Int] -> Int
-- that returns a fold of all the elements that satisfy the given predicate.
filterFoldl fb f x list = foldl f x (filter fb list)

insert :: (Int -> Int -> Bool) -> [Int] -> Int -> [Int]
--that, given a relation between integers, a list and un element, return the list with the inserted element according to the relation.
insert _ [] x = [x]
insert op (head:tail) x
    |op x head = x:head:tail
    |otherwise = head:(insert op tail x)

insertionSort :: (Int -> Int -> Bool) -> [Int] -> [Int]
 --Use function insert, in order to define function  that orders a list according to the given relation.

insertionSort _ [] = []
insertionSort op list = foldl (insert op) [] list