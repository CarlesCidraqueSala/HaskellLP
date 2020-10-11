insert :: [Int] -> Int -> [Int] 
    --that, given a sorted list and an element, correctly inserts the new element in the list.
insert [] n = [n]
insert totllista@(x:xs) y
	| x >= y = y:x:xs
	| maximum totllista < y =  x:xs ++ [y]
	| otherwise = [x] ++ insert xs y

isort :: [Int] -> [Int] 
    --that implements insertion sort using the previous function.
isort [] = []
isort (x:xs) = (insert (isort xs) x)

remove :: [Int] -> Int -> [Int] 
    --that, given a list and an element x, erases the first occurrence of x from the list. You can assume that the element is always in the list.
remove(x:xs) e
    | x == e = xs
    | otherwise = [x] ++ remove xs e

ssort :: [Int] -> [Int] 
    --that implements selection sort using the previous function.
ssort [] = []
ssort xs = (minimum xs):ssort(remove xs (minimum xs))

merge :: [Int] -> [Int] -> [Int] 
    --that, given two sorted lists, merges them to get a list with all the elements in sorted order.
merge [] [] = []
merge [] xs = xs
merge xs [] = xs
merge tot1@(x:xs) tot2@(y:ys)
    | x > y = y:(merge tot1 ys)
    |otherwise = x:(merge xs tot2)


msort :: [Int] -> [Int] 
    --that implements merge sort using the previous function.
msort [] = []
msort [x] = [x]
msort l = merge (msort l1) (msort l2)
    where ll = splitAt (div (length l) 2) l 
          l1 = fst ll
          l2 = snd ll

numb :: Int -> [Int] -> ([Int],[Int])
numb _ [] = ([],[])
numb n (y:ys)
	| y < n = (y:l1,l2)
	| otherwise = (l1,y:l2)
		where (l1,l2) = numb n ys
qsort :: [Int] -> [Int] 
    --that implements quick sort.	
qsort [] = []
qsort [x] = [x]
qsort (x:xs) = qsort(fst (numb x xs)) ++ [x] ++ qsort(snd (numb x xs))

genQsort :: Ord a => [a] -> [a] 
    --that sorts elements of any type.
genQsort [] = []
genQsort [x] = [x]
genQsort (x:xs) = genQsort(fst (numb x xs)) ++ [x] ++ genQsort(snd (numb x xs))
    where
        numb :: Ord a => a -> [a] -> ([a],[a])
        numb _ [] = ([],[])
        numb n (y:ys)
            | y < n = (y:l1,l2)
            | otherwise = (l1,y:l2)
            where (l1,l2) = numb n ys
