{-The goal of this problem is to work the definition of infinite lists. In particular, 
you are required to define functions that generate infinite lists to: -}

{-In this problem you cannot use infinite enumerations such as [1..], 
but you are advised to use higer-order functions such as map, scanl, iterate, filter, ... -}

-- Generate the sequence of ones [1,1,1,1,1,1,1,1,…].

ones :: [Integer]

ones = [1] ++ ones

--Generate the sequence of the natural numbers [0,1,2,3,4,5,6,7…].

nats :: [Integer]

nats = iterate (+1) 0

--Generate the sequence of the integer numbers [0,1,−1,2,−2,3,−3,4…].

ints :: [Integer]

ints = iterate (integer) 0
    where
        integer :: Integer -> Integer
        integer x
            | x > 0 = (-x)
            |otherwise = (-x) + 1

-- Generate the sequence of the triangular numbers: 0,1,3,6,10,15,21,28,…].
-- is equal to the sum of the n natural numbers from 1 to n
triangulars :: [Integer]

triangulars = triangle 0
     where
          triangle :: Integer -> [Integer]
          triangle x = div (x * (x + 1)) 2 : triangle (x + 1)

--Generate the sequence of the factorial numbers: [1,1,2,6,24,120,720,5040,…].

factorials :: [Integer]

factorials = facts 0
     where
          facts :: Integer -> [Integer]
          facts x = factorial x : facts (x + 1)
               where
                    factorial :: Integer -> Integer
                    factorial 0 = 1
                    factorial x = x * factorial (x - 1)

--Generate the sequence of the Fibonacci numbers: [0,1,1,2,3,5,8,13,…].

fibs :: [Integer]

fibs = 0 : 1 : zipWith (+) fibs (tail fibs)

--Generate the sequence of prime numbers: [2,3,5,7,11,13,17,19,…].

isPrimeRec :: Integer -> Integer -> Bool
isPrimeRec x d
    | d == 1 = True
    | mod x d == 0 = False
    | otherwise = isPrimeRec x (d - 1)

isPrime :: Integer -> Bool
isPrime x
    | x == 0 = False
    | x == 1 = False
    | otherwise = isPrimeRec x (floor (sqrt (fromIntegral x)))

primes :: [Integer]
primes = primers 2
     where
          primers :: Integer -> [Integer]
          primers x
               | isPrime x = x : primers (x + 1)
               | otherwise = primers (x + 1)

--Generate the ordered sequence of the Hamming numbers: [1,2,3,4,5,6,8,9,…]. 
--The Hamming numbers are those that only have 2, 3 and 5 as prime divisors.

hammings :: [Integer]
hammings = 1 : merge3 (map (* 2) hammings) (map (* 3) hammings) (map (* 5) hammings)

merge3 :: [Integer] -> [Integer] -> [Integer] -> [Integer]
merge3 xs ys zs = merge2 (merge2 xs ys) zs

merge2 :: [Integer] -> [Integer] -> [Integer] 
merge2 a [] = a
merge2 [] b = b
merge2 (a:as) (b:bs)
      | a < b  = a : (merge2 as (b:bs))
      | b < a  = b : (merge2 (a:as) bs)
      |otherwise = merge2 (a:as) bs

--Generate the look-and-say sequence: [1,11,21,1211,111221,312211,13112221,1113213211,…].

lookNsay :: [Integer]
lookNsay = iterate count 1

count :: Integer -> Integer
count a = read $ next $ show a

next :: [Char] -> [Char]
next [] = []
next cs = (show n) ++ [pr] ++ next cua
  where 
    pr = head cs
    n = length $ takeWhile ( == pr) cs
    cua = dropWhile ( == pr) cs

--Generate the sequences of rows of the Tartaglia triangle (also known as Pascal’s triangle): [[1],[1,1],[1,2,1],[1,3,3,1],…].

tartaglia :: [[Integer]]
tartaglia = (iterate (pascal) [1])

pascal :: [Integer] -> [Integer]
pascal xs = zipWith (+) (xs ++ [0]) ([0] ++ xs)


