factorial :: Integer -> Integer
    -- factorial retorna el factorial d'un nombre natural donat

factorial 0 = 1;
factorial n = n * factorial(n-1)

doblar x = 2 * x

triplicar x = x * 3
