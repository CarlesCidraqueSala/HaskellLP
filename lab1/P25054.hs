myLength :: [Int] -> Int 
    --that, given a list of integers, returns its length.
myLength [] = 0
myLength (_:xs) = 1 + myLength xs

myMaximum :: [Int] -> Int
    -- that, given a non-empty list of integers, returns its maximal element.
myMaximum xs = maximum xs

average :: [Int] -> Float
    --that, given a non-empty list of integers, returns its average.
    --div fromIntegral((sum xs)) fromIntegral((myLength xs))
average xs = fromIntegral(sum xs) / fromIntegral(myLength xs)

buildPalindrome :: [Int] -> [Int]
    --that, given a list, returns its palindrome that starts with the reserved list.
buildPalindrome xs = reverse xs ++ xs

remove :: [Int] -> [Int] -> [Int] 
    --that given a list of integers x and a list of integers y, returns x after having removed all the ocurrences of the elements in y.
remove l1 [] = l1
remove l1 (x:l2) = remove (remove' l1 x) l2
    where 
        remove' :: [Int] -> Int -> [Int]
        remove' [] _ = []
        remove' (x:l) y
            | x == y = remove' l y
            | otherwise = x:(remove' l y)

flatten :: [[Int]] -> [Int]
    -- that flattens a list of lists yielding a single list of elements. Es un concatenar
flatten [] = []
flatten [lastArray] = lastArray
flatten (head:tail) = head ++ flatten tail

oddsNevens :: [Int] -> ([Int],[Int]) 
    --that, given a list of integers, returns two lists: One with all the even numbers and one with all the odd numbers, 
    --each of them in the same relative order as in the original list.
oddsNevens [] = ([],[])
oddsNevens array = (oddsNevens' array [] [])
    where
        oddsNevens' :: [Int] -> [Int] -> [Int] -> ([Int], [Int])
        oddsNevens' [] odds evens = (odds, evens)
        oddsNevens' (head:tail) odds evens
            | even head = (oddsNevens' tail odds (evens ++ [head]))
            | otherwise = (oddsNevens' tail (odds ++ [head]) evens)


primeDivisors :: Int -> [Int]
    --that returns the list of prime divisors of a non-zero natural.
primeDivisors a = primeDivisorsRec 2
    where
        primeDivisorsRec :: Int -> [Int]
        primeDivisorsRec n
            | n == a + 1 = []
            | mod a n == 0 && isPrime n   = n : primeDivisorsRec (n+1)
            | otherwise = primeDivisorsRec (n+1)
            where
                isPrime :: Int -> Bool
                isPrime 0 = False
                isPrime 1 = False
                isPrime n = not (hasDivisors (n-1))
                    where
                        hasDivisors :: Int -> Bool
                        hasDivisors 1 = False
                        hasDivisors x = mod n x == 0 || hasDivisors (x-1)
