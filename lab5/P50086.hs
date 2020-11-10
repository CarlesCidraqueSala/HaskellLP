------------------------------------------Ex Queue1(1)
data Queue a = Queue [a] [a]
     deriving (Show)
create :: Queue a

create = Queue [] []

push :: a -> Queue a -> Queue a
push x (Queue a b) = (Queue a (x:b))
        
pop :: Queue a -> Queue a
pop (Queue [] l2) = (Queue (drop 1 l1) [])
  where l1 = myReverse l2
pop (Queue l1 l2) = (Queue (drop 1 l1) l2)

empty :: Queue a -> Bool
empty (Queue [] []) = True
empty (Queue l1 l2) = False

top :: Queue a -> a
top (Queue [] l2) = last l2
top (Queue (x:l1) l2) = x
           

instance (Eq a) => Eq (Queue a) where
  Queue [][] == Queue [][] = True
  q1 == q2  
    | (empty q1 || empty q2) = False
    | otherwise = if (top q1) == (top q2) 
             then ((pop q1) == (pop q2)) 
             else False

myReverse :: [a] -> [a]
myReverse = foldl (\l x -> ([x] ++ l)) []
        
sizeQ :: Queue a -> Int
sizeQ (Queue l1 l2) = (length l1) +  (length l2)
------------------------------------------End Queue(1)

{-
Feu que Queue sigui instància de la classe Functor. Per això implementeu la funció fmap que, 
donada una funció de tipus p -> q i un Queue d’elements de tipus p, 
retorna un Queue de tipus q resultant d’aplicar la funció a tots els elements de la cua.
-}

instance Functor Queue
    where
        fmap fn (Queue q1 q2) = (Queue (fmap (fn) q1) (fmap (fn) q2))

{-
Feu una funció translation :: Num b => b -> Queue b -> Queue b 
que aplica una translació a tots els punts d’una cua (que serà el segon paràmetre).
-}

translation :: Num b => b -> Queue b -> Queue b
translation f (Queue b1 b2) = (Queue b1 (fmap (+ f) b2))

{-
Feu que Queue sigui instància de la classe Monad. 
Per a resoldre aquest apartat, pot ser útil fer una operació que faci la unió de dues cues del mateix tipus.
-}

q2l :: (Queue a) -> [a]
q2l (Queue l1 l2) = (l1 ++ (reverse l2))

instance Applicative Queue
    where
        pure x = (Queue [x] [])
        qfn <*> q = (Queue l [])
            where
                l = (q2l qfn) <*> (q2l q)

instance Monad Queue
    where
        return x = (Queue [x] [])
        q >>= fn = (Queue l [])
            where
                l = (q2l q) >>= (q2l . fn)

{-
Feu, utilitzant la notació do, una funció kfilter :: (p -> Bool) -> Queue p -> Queue p 
que selecciona tots els elements d’una cua que satisfan una propietat donada.
-}
kfilter :: (p -> Bool) -> Queue p -> Queue p
kfilter fn q = do
    x <- q
    if (fn x) then return x else (Queue [] [])