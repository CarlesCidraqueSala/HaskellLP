----------------------------------------------------------
-- PRACT HASKELL HS-DTS. DECISION TREES
-----------------------------------------------------------
{-
Attribute Information:

1. cap-shape: bell=b,conical=c,convex=x,flat=f, knobbed=k,sunken=s
2. cap-surface: fibrous=f,grooves=g,scaly=y,smooth=s
3. cap-color: brown=n,buff=b,cinnamon=c,gray=g,green=r, pink=p,purple=u,red=e,white=w,yellow=y
4. bruises?: bruises=t,no=f
5. odor: almond=a,anise=l,creosote=c,fishy=y,foul=f, musty=m,none=n,pungent=p,spicy=s
6. gill-attachment: attached=a,descending=d,free=f,notched=n
7. gill-spacing: close=c,crowded=w,distant=d
8. gill-size: broad=b,narrow=n
9. gill-color: black=k,brown=n,buff=b,chocolate=h,gray=g, green=r,orange=o,pink=p,purple=u,red=e, white=w,yellow=y
10. stalk-shape: enlarging=e,tapering=t
11. stalk-root: bulbous=b,club=c,cup=u,equal=e, rhizomorphs=z,rooted=r,missing=?
12. stalk-surface-above-ring: fibrous=f,scaly=y,silky=k,smooth=s
13. stalk-surface-below-ring: fibrous=f,scaly=y,silky=k,smooth=s
14. stalk-color-above-ring: brown=n,buff=b,cinnamon=c,gray=g,orange=o, pink=p,red=e,white=w,yellow=y
15. stalk-color-below-ring: brown=n,buff=b,cinnamon=c,gray=g,orange=o, pink=p,red=e,white=w,yellow=y
16. veil-type: partial=p,universal=u
17. veil-color: brown=n,orange=o,white=w,yellow=y
18. ring-number: none=n,one=o,two=t
19. ring-type: cobwebby=c,evanescent=e,flaring=f,large=l, none=n,pendant=p,sheathing=s,zone=z
20. spore-print-color: black=k,brown=n,buff=b,chocolate=h,green=r, orange=o,purple=u,white=w,yellow=y
21. population: abundant=a,clustered=c,numerous=n, scattered=s,several=v,solitary=y
22. habitat: grasses=g,leaves=l,meadows=m,paths=p, urban=u,waste=w,woods=d
-}

import Data.Maybe
import Data.List
import Data.Tree (drawTree)
import Control.Monad.Trans.Cont (shift)
import Distribution.Parsec
import Control.Monad.State

--definició de l'estructura del decision tree
type AttName = String

type AttValue = String

type Attribute = (AttName, [AttValue])

type Header = [Attribute]

type Row = [AttValue]

type DataSet = (Header, [Row])

data DecisionTree = Null |
                    Leaf AttValue | 
                    Node AttName [(AttValue, DecisionTree)]
                  deriving (Eq, Show)
                
-----------LLegenda pels atributs
type LlegendaAtt = (String, [String])

type HeaderLlegendaAtt = [LlegendaAtt]
-----------------

-----FUNCIONS AUXIALIARS BÀSIQUES
type Partition = [(AttValue, DataSet)]

-- AttSelector es una funció que selecciona el següent atribut donats un DataSet i l'atribut de clasificació
type AttSelector = DataSet -> Attribute -> Attribute

-- La funció xlogx retorna el resultat de computar x * log2 x per una x pasada com a paràmetre
xlogx :: Double -> Double
xlogx p
  | p <= 1e-100 = 0.0
  | otherwise   = p * log2 p 
  where
    log2 x = log x / log 2

-- La funció lookUp busca el valor d'un objecte de tipus 'a' dins de la llista de pairs (a, b) retornem el valor 'b' associat
lookUp :: (Eq a, Show a, Show b) => a -> [(a, b)] -> b
lookUp x table
  = fromMaybe (error ("lookUp error - no binding for " ++ show x ++ 
                      " in table: " ++ show table))
              (lookup x table)
---------
---------------definició llegenda dels diferents attributes
{-
type LlegendaAtt = (String, [String])

type HeaderLlegendaAtt = [LlegendaAtt]
-----------------
-}
llegendaA :: HeaderLlegendaAtt
llegendaA = [("class", ["poisounous=p", "edible=e"]), ("capShape", ["bell=b", "conical=c", "convex=x", "flat=f", "knobbed=k", "sunken=s"]), ("capSurface", ["fibrous=f", "grooves=g", "scaly=y", "smooth=s"]), ("capColor", ["brown=n", "buff=b", "cinnamon=c", "gray=g", "green=r", "pink=p", "purple=u", "red=e", "white=w", "yellow=y"]),
 ("bruises", ["bruises=t", "no=f"]), ("odor", ["almond=a", "anise=l", "creosote=c", "fishy=y", "foul=f", "musty=m", "none=n", "pungent=p","spicy=s"]), ("gillAttachment", ["attached=a","descending=d","free=f","notched=n"]), ("gillSpacing", ["close=c","crowded=w","distant=d"]),
 ("gillSize", ["broad=b","narrow=n"]), ("gillColor", ["black=k","brown=n","buff=b","chocolate=h","gray=g", "green=r","orange=o","pink=p","purple=u","red=e", "white=w","yellow=y"]), ("stalkShape", ["enlarging=e","tapering=t"]), ("stalkRoot", ["bulbous=b","club=c","cup=u","equal=e", "rhizomorphs=z","rooted=r","missing=?"]),
  ("stalkSurfaceAboveRing", ["fibrous=f","scaly=y","silky=k","smooth=s"]), ("stalkSurfaceBelowRing", ["fibrous=f","scaly=y","silky=k","smooth=s"]), ("stalkColorAboveRing", ["brown=n","buff=b","cinnamon=c","gray=g","orange=o", "pink=p","red=e","white=w","yellow=y"]),
   ("stalkColorBelowRing", ["brown=n","buff=b","cinnamon=c","gray=g","orange=o", "pink=p","red=e","white=w","yellow=y"]), ("veilType", ["partial=p","universal=u"]), ("veilColor", ["brown=n","orange=o","white=w","yellow=y"]), ("ringNumber", ["none=n","one=o","two=t"]),
    ("ringType", ["cobwebby=c","evanescent=e","flaring=f","large=l", "none=n","pendant=p","sheathing=s","zone=z"]), ("sporePrintColor", ["black=k","brown=n","buff=b","chocolate=h","green=r", "orange=o","purple=u","white=w","yellow=y"]), 
    ("population", ["abundant=a","clustered=c","numerous=n", "scattered=s","several=v","solitary=y"]), ("habitat", ["grasses=g","leaves=l","meadows=m","paths=p", "urban=u","waste=w","woods=d"])]

---------------definició dels diferents attributes
--class: poisounous=p,edible=e
classifPE :: Attribute
classifPE 
  = ("class", ["p", "e"])
--1. cap-shape: bell=b,conical=c,convex=x,flat=f, knobbed=k,sunken=s
capShape :: Attribute 
capShape 
  = ("capShape", ["b", "c", "x", "f", "k", "s"])
--2. cap-surface: fibrous=f,grooves=g,scaly=y,smooth=s
capSurface :: Attribute 
capSurface 
  = ("capSurface", ["f", "g", "y", "s"])
--3. cap-color: brown=n,buff=b,cinnamon=c,gray=g,green=r, pink=p,purple=u,red=e,white=w,yellow=y
capColor :: Attribute 
capColor 
  = ("capColor", ["n", "b", "c", "g", "r", "p", "u", "e", "w", "y"])
--4. bruises?: bruises=t,no=f
bruises :: Attribute 
bruises
  = ("bruises", ["t", "f"])
--5. odor: almond=a,anise=l,creosote=c,fishy=y,foul=f, musty=m,none=n,pungent=p,spicy=s
odor :: Attribute 
odor
  = ("odor", ["a", "l", "c", "y", "f", "m", "n", "p", "s"])
--6. gill-attachment: attached=a,descending=d,free=f,notched=n
gillAttachment :: Attribute 
gillAttachment 
  = ("gillAttachment", ["a", "d", "f", "n"])
--7. gill-spacing: close=c,crowded=w,distant=d
gillSpacing :: Attribute 
gillSpacing 
  = ("gillSpacing", ["c", "w", "d"])
--8. gill-size: broad=b,narrow=n
gillSize :: Attribute 
gillSize 
  = ("gillSize", ["b", "n"])
--9. gill-color: black=k,brown=n,buff=b,chocolate=h,gray=g, green=r,orange=o,pink=p,purple=u,red=e, white=w,yellow=y
gillColor :: Attribute 
gillColor 
  = ("gillColor", ["k", "n", "b", "h", "g", "r", "o", "p", "u", "e", "w", "y"])
--10. stalk-shape: enlarging=e,tapering=t
stalkShape :: Attribute 
stalkShape 
  = ("stalkShape", ["e", "t"])
--11. stalk-root: bulbous=b,club=c,cup=u,equal=e, rhizomorphs=z,rooted=r,missing=?
stalkRoot :: Attribute 
stalkRoot 
  = ("stalkRoot", ["b", "c", "u", "e", "z", "r", "?"])
--12. stalk-surface-above-ring: fibrous=f,scaly=y,silky=k,smooth=s
stalkSurfaceAboveRing :: Attribute 
stalkSurfaceAboveRing 
  = ("stalkSurfaceAboveRing", ["f", "y", "k", "s"])
--13. stalk-surface-below-ring: fibrous=f,scaly=y,silky=k,smooth=s
stalkSurfaceBelowRing :: Attribute 
stalkSurfaceBelowRing 
  = ("stalkSurfaceBelowRing", ["f", "y", "k", "s"])
--14. stalk-color-above-ring: brown=n,buff=b,cinnamon=c,gray=g,orange=o, pink=p,red=e,white=w,yellow=y
stalkColorAboveRing :: Attribute 
stalkColorAboveRing 
  = ("stalkColorAboveRing", ["n", "b", "c", "g", "o", "p", "e", "w", "y"])
--15. stalk-color-below-ring: brown=n,buff=b,cinnamon=c,gray=g,orange=o, pink=p,red=e,white=w,yellow=y
stalkColorBelowRing :: Attribute 
stalkColorBelowRing 
  = ("stalkColorBelowRing", ["n", "b", "c", "g", "o", "p", "e", "w", "y"])
--16. veil-type: partial=p,universal=u
veilType :: Attribute 
veilType 
  = ("veilType", ["p", "u"])
--17. veil-color: brown=n,orange=o,white=w,yellow=y
veilColor :: Attribute 
veilColor 
  = ("veilColor", ["n", "o", "w", "y"])
--18. ring-number: none=n,one=o,two=t
ringNumber :: Attribute 
ringNumber 
  = ("ringNumber", ["n", "o", "t"])
--19. ring-type: cobwebby=c,evanescent=e,flaring=f,large=l, none=n,pendant=p,sheathing=s,zone=z
ringType :: Attribute 
ringType 
  = ("ringType", ["c", "e", "f", "l", "n", "p", "s", "z"])
--20. spore-print-color: black=k,brown=n,buff=b,chocolate=h,green=r, orange=o,purple=u,white=w,yellow=y
sporePrintColor :: Attribute 
sporePrintColor 
  = ("sporePrintColor", ["k", "n", "b", "h", "r", "o", "u", "w", "y"])
--21. population: abundant=a,clustered=c,numerous=n, scattered=s,several=v,solitary=y
population :: Attribute 
population 
  = ("population", ["a", "c", "n", "s", "v", "y"])
--22. habitat: grasses=g,leaves=l,meadows=m,paths=p, urban=u,waste=w,woods=d
habitat :: Attribute 
habitat 
  = ("habitat", ["g", "l", "m", "p", "u", "w", "d"])                       
--------------------------------------------------------------------
-- La funció allSame retorna True si tots els elements d'una llista pasada son iguals
allSame :: Eq a => [a] -> Bool
allSame as
  | (a:as') <- as = and ( zipWith (==) (a : as') (as' ++ [a]) )
  | otherwise     = True

-- La funció remove elimina un element de tipus 'a' d'una llista de pairs (a, b), si no el trobem retornem la llista sense modificar
remove :: Eq a => a -> [(a, b)] -> [(a, b)]
remove a table 
  = filter ( (a /=) . fst ) table

-- La funció lookUpAtt busca el valor d'un atribut pasat com a paràmetre en una Row determinada
lookUpAtt :: AttName -> Header -> Row -> AttValue
lookUpAtt att header row
  = lookUp att (zipWith f header row)
  where 
    f a b = (fst a, b)

-- La funció removeAtt elimina el valor del atribut pasat com a paràmetre d'una Row determinada
removeAtt :: AttName -> Header -> Row -> Row
removeAtt att header row 
  = delete (lookUpAtt att header row) row

-- La funció buildFrequencyTable construeix una taula de frequencies que compte el número d'ocurrències de cada valor d'un atribut en el DataSet pasat com a paràmetre
buildFrequencyTable :: Attribute -> DataSet -> [(AttValue, Int)]
buildFrequencyTable (attName, (attValues)) (header, rows)
  = [ (a, n) | a <- attValues, n <- [length (filter (a ==) atts)] ]
  where 
    atts = map (lookUpAtt attName header) rows

--La funció partitionData fa la partició del DataSet pasat com a paràmetre a partir del atribut indicat
partitionData :: DataSet -> Attribute -> Partition
partitionData (h, rs) (a, attVals)
  = [ (attVal, (remove a h,  [ removeAtt a h r | r <- rs, lookUpAtt a h r == attVal ]  )) | attVal <- attVals ]

-- La funció builTree construeix un dt a partir d'un DataSet pasat com a paràmetre utilitzant la funció AttSelector, l'atribut que fa de clasificador es l'atribut pasat com a paràmetre.
buildTree :: DataSet -> Attribute -> AttSelector -> DecisionTree 
buildTree (_,[]) _ _
  = Null
buildTree dataSet@(h,rs) classifierAtt@(classifierName, _) attSelector
  | allSame (map (lookUpAtt classifierName h) rs) = Leaf (lookUpAtt classifierName h (rs !! 0))
  | otherwise = Node attName [ (a, t) | a <- attValues, t <- [buildTree (lookUp a partition) classifierAtt attSelector] ]
  where 
    att@(attName, attValues) = attSelector dataSet classifierAtt
    partition = partitionData dataSet (att)
--------------------------------------------------------------------
-- La funció entropy computa la entropia d'un DataSet pasat com a paràmetre respecte l'atribut pasat; la entropia d'un DataSet buit es = 0
entropy :: DataSet -> Attribute -> Double
entropy (_, []) _ 
  = 0
entropy dataSet@(h, rs) att@(_, vals)
  = sum (map (negate . xlogx) (map (px dataSet att) vals))

px :: DataSet -> Attribute -> AttValue -> Double
px dataSet@(_, rs) att x 
  = (fromIntegral (lookUp x (buildFrequencyTable att dataSet))) / (fromIntegral (length rs))

-- La funció gain computa la information gain d'un DataSet pasat respecte un atribut de partició inicial i l'atribut de classificació (class)
gain :: DataSet -> Attribute -> Attribute -> Double
gain dataSet@(h, rs) att@(_, vals) classifierAtt
  = entropy dataSet classifierAtt - sum (map f vals) 
  where 
    f val = (px dataSet att val) * entropy (lookUp val (partitionData dataSet att)) classifierAtt

-- La funció bestGainAtt selecciona l'atribut que obté el information gain més elevat (sense tenir en compte l'atribut de classificació class),
-- en cas d'empat s'agafa el primer trobat amb aquest màxim.
bestGainAtt :: AttSelector
bestGainAtt dataSet@(header, _) classifierAtt@(classifierName, _)
  = lookUp (maximum (map fst gains)) gains
  where 
    gains = [ (gain dataSet att classifierAtt, att) | att <- (remove classifierName header) ]

--------------------------------------------------------------------
--------------------Funcio per fer split dels atributs de cada linia del fitxer d'entrada

splitByDelimiter :: String -> [String]
splitByDelimiter "" = []
splitByDelimiter list =
  map (takeWhile (/= ',') . tail)
    (filter (isPrefixOf [','])
       (tails
           (',' : list)))
--------------------
--------------------------- searchAtt retorna els posibles valors que pot tenir un atribut determinat pasat com a paràmetre
searchAtt :: AttName -> Header -> [AttValue]
searchAtt _ [] = error "element not found"
searchAtt x ((a,b):xs) = if x == a then b else searchAtt x xs
--------------------------------------------------------------------

--------------------------- searchLLegenda retorna la llegenda dels valors que pot tenir el atribut pasat com a paràmetre
searchLLegenda :: String -> HeaderLlegendaAtt -> [String]
searchLLegenda _ [] = error "element not found"
searchLLegenda x ((a,b):xs) = if x == a then b else searchLLegenda x xs

--------------------------- searchSegDT retorna el decision tree que te com a arrel el node amb valor el AttValue pasat com a paràmetre
searchSegDT :: AttValue -> [(AttValue, DecisionTree)] -> DecisionTree
searchSegDT _ [] = error "element not found"
searchSegDT x ((a,b):xs) = if x == a then b else searchSegDT x xs

--------------------------- printDT imprimeix l'arbre passat com a paràmetre (més info al README.md)
printDT :: DecisionTree  -> String -> [String]  -> IO ()
printDT Null atVal stringAcumulat = do
  print(atVal)
  print(stringAcumulat ++ ["---+" ++ "  |" ++ "  Null"])

printDT (Leaf attVal) atVal stringAcumulat = do
  print(atVal)
  print(stringAcumulat ++ ["---+" ++ "  | " ++ attVal])

printDT (Node x paths) atVal stringAcumulat = do 
  let stringAcumulatAux = (stringAcumulat ++ ["---+"]) :: [String]
  let llistaDT = [dtt | (_, dtt) <- paths]
  let llistadtAttVal = [dtattval | (dtattval, _) <- paths]
  print(atVal)
  print(stringAcumulatAux ++ [x] )
  mapM_ (\(a, b) -> printDT a b stringAcumulatAux) $ zip llistaDT llistadtAttVal
--------------------------
{-
La funció "preguntar" recorrer el dt pasat com a paràmetre segons les respostes que vagi triant l'usuari.
En cas que arribem a un valor null vol dir que l'arbre no te solució per aquella combinació de respostes de l'usuari.
En cas de que arribem a una Leaf donem com a resposta el valor que te.
El pas recursiu es cridar al subarbre del dt que estem analitzant anant cap al fill que l'usuari a triat mitjançant el valor d'un atribut.
-}
preguntar :: DecisionTree -> Header -> IO ()

preguntar (Null) header = do
    putStrLn "No es pot predir el resultat"
    

preguntar (Leaf attVal) header = do
  if attVal == "p" then
    putStr "Aquesta es la predicció : poisonous"
  else putStr "Aquesta es la predicció : edible"
    --putStr ("Aquesta es la predicció : " ++ attVal)
    
preguntar (Node attName paths) header = do
    putStr ("Quin es el valor de :  " ++ attName ++ " ? " ++ "tenim les següents opcions : ")
    print (searchAtt attName header)
    putStr ("Llegenda per a l'atribut  " ++ "'" ++ attName ++ "' : ")
    print(searchLLegenda attName llegendaA)
    atval <- getLine
    putStr ("El valor escollir per a l'atribut " ++ attName ++ " es : " ++ atval)
    putStrLn "\n"
    -- D'entre els paths = [(AttValue, DecisionTree)], hem d'agafar aquell que tingui el AttValue igual al que ha pasat el usuari
    let dt = searchSegDT atval paths :: DecisionTree
    if(numFulles dt == 0) then putStrLn "No es pot predir el resultat"
    else preguntar dt header

-- numFulles indica quantes fulles te un arbre determinat, es a dir, tenen un valor de "edible" o "poisonous"
numFulles :: DecisionTree -> Int
numFulles Null
  = 0
numFulles (Leaf _)
  = 1
numFulles (Node _ x)
  = sum (map (numFulles . snd) x)
--------------------
--------------------
--------------------
main :: IO ()
main = do
  contents <- readFile "agaricus-lepiota.data"
  --INICIALITZACIÓ D'ESTRUCTURA del DataSet per poder-lo tractar
  let linesContents = lines contents :: [String] -- aqui tenim [Row]
  let header = [classifPE, capShape, capSurface, capColor, bruises, odor, gillAttachment, gillSpacing, gillSize, gillColor, stalkShape, stalkRoot, stalkSurfaceAboveRing, stalkSurfaceBelowRing, stalkColorAboveRing, stalkColorBelowRing, veilType, veilColor, ringNumber, ringType, sporePrintColor, population, habitat] :: Header
  let table = map splitByDelimiter linesContents --lines de la entrada "agaricus-lepiota.data", cada fila (line) es una row (type Row = [AttValue])
  let mushroomData = (header, table) :: DataSet 
  ------------------------------------
  --obtenim el DT que te com a arrel el node que té com a atribut el que millor predicció fa
  let fig2MushroomData = buildTree mushroomData classifPE bestGainAtt :: DecisionTree
  ------------------------------------
  print ("Vols visualitzar el decision tree ? Respon 'si' o 'no'")
  resposta <- getLine
  if (resposta == "si") then printDT fig2MushroomData "" []
  else putStrLn "\n"
  putStrLn "------------------------------"
  preguntar fig2MushroomData header
  putStrLn "\n"
  return()