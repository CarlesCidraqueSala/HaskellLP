absValue :: Int -> Int
    -- that, given an integer, returns its absolute value.

absValue n
    | n >= 0 = n
    | otherwise = -n


power :: Int -> Int -> Int
    -- that, given an integer x and a natural p, returns thep-th power of x, that is, x^p.

power x y = x ^ y

isPrime :: Int -> Bool
    -- that, given a natural, tells whether it is a prime number or not.
isPrime 0 = False
isPrime 1 = False
isPrime 2 = True
isPrime x =   (0 == length [y | y<-[2..x-1], mod x y == 0])

slowFib :: Int -> Int
    --returns the n-th element of the Fibonacci sequence using the recursive algorithm that defines it (f(0)=0, f(1)=1, f(n)=f(n−1)+f(n−2) for n≥ 2).
slowFib 0 = 0
slowFib 1 = 1
slowFib n = slowFib(n-1) + slowFib(n-2)

fib2 :: Int-> Int -> Int -> Int
fib2 x y num 
	| num /= 0 = fib2 y (x+y) (num-1)
	| otherwise = x

quickFib :: Int -> Int 
    -- that returns the n-th element of the Fibonacci sequence using a more efficient algorithm.
quickFib x = fib2 0 1 x



