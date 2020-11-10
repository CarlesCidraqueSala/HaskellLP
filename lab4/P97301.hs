{- 
In the famous article Using FizzBuzz to Find Developers who Grok Coding 
(which you can find at http://imranontech.com/2007/01/24/using-fizzbuzz-to-find-developers-who-grok-coding/), 
Imran Ghory explains that most programmers interviewed for a job cannot even write a simple program
For multiples of three print ‘Fizz’ instead of the number, and for the multiples of five print ‘Buzz’. 
For numbers which are multiples of both three and five print ‘FizzBuzz’.
-}

fizzBuzz :: [Either Int String]

fizzBuzz = map fizzBuzzAux [0..]

fizzBuzzAux :: Int -> Either Int String

fizzBuzzAux x
    | (mod x 3 == 0) && (mod x 5 == 0) = Right "FizzBuzz" 
    | (mod x 3 == 0) = Right "Fizz" 
    | (mod x 5 == 0) = Right "Buzz" 
    | otherwise = Left x  


