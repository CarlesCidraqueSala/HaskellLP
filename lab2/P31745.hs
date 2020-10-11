 --Implement the following functions using higher-order functions (and other predefined functions) of Haskell without using recursion.
flatten :: [[Int]] -> [Int]
    --that flattens a list of lists of integers in a list of integers.
flatten [] = []
--flatten [_] = [_]
flatten llistaGran = foldl (++) [] llistaGran --es podria haver fet servir la funció "concat"

myLength :: String -> Int
    --that returns the length of a string.
myLength paraula = length paraula
--myLength a = foldl (\y x -> y + 1) 0 a

myReverse :: [Int] -> [Int]
    --that reverses a list of integers.
myReverse llistaR = reverse llistaR
--myReverse llista = foldr (\x y -> y++[x]) [] llista

countIn :: [[Int]] -> Int -> [Int]
    --that, given a list of sublists ℓ and an element x, returns the list that tells who many times x appears in each sublist of ℓ.
countIn x a = map (\l -> length (filter (== a) l)) x


firstWord :: String -> String 
    --    that, given a string with blanks and alphabetic characters, returns its first word.
firstWord x = takeWhile (/= ' ') (dropWhile (== ' ') x) --primer treiem els blanks del davant i posteriorment tot el demés després de la primera paraula
