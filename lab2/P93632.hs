eql :: [Int] -> [Int] -> Bool   
    --that tells wether two lists of integers are equal. APLICAR UN ZipWith (FOS), tot i que no es correcte pq de ZipWith obtenim una llista com a res.

    --El que fem a continuació es donades dos llistes a i b, primer mirar que aquestes dues llistes tinguin la mateixa llargada:
        -- length a == length b
    --Després el que cal mirar es si tots els elements de la llista a i b son iguals, per ferho utilitzem dues FOS, "all" i "ZipWith"
        -- La FOS "all" la utilitzem per aplicar que tots els elements del resultat de ZipWith compleixin el predicat (==True),
        -- El ZipWith el que fa es aplicar l'op "==" entre cadascun dels elements de la llista a i la llista b,
        --així obtenint una llista resultant en aquest cas de Bool's.
    --Per tant el resultat de la funció eql utilitzant aquestes dues FOS seria:
eql a b = length a == length b && all (==True) (zipWith (==) a b)

prod :: [Int] -> Int
    --that returns the product of a list of integers.
--Aplicar la FOS foldl que desplega un operador per l'esquerra, com que es el producte podem posar com a numero base a operar el numero 1,
-- pot quedar una mica lleig, pero hauria de funcionar... El resultat es el correcte
prod llista = foldl (*) 1 llista

prodOfEvens :: [Int] -> Int
    --that returns the product of all even numbers of a list of integers.
--Primer caldria fer un filter even, per així només operar sobre els even numbers de la llista donada, un cop fet, aplicar foldl (*)
prodOfEvens llista = foldl (*) 1 (filter even llista)

powersOf2 :: [Int]
    --that generates the list of all the powers of 2.
--Seria aplicar la iterate FOS, llavors en la sentencia d'execució caldria fer un take de la llista, si no el programa es penjaria.
powersOf2 = iterate (*2) 1

scalarProduct :: [Float] -> [Float] -> Float
    --that returns the dot product of two lists of float numbers with the same size.
--Ho podem fer mitjançant primer un zipWith, on primer fem el producte i després, amb un foldl la suma del array resultant
scalarProduct llista1 llista2 = foldl (+) 0 (zipWith (*) llista1 llista2)


