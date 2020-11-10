--We want to represent queues to improve the efficiency of its push and pop operations. In order to do so, we implement queues through two lists, so that if we concatenate the first with the reverse of the second, we get the elements of the queue if exit order. 
--Using a Queue constructor for this type,
q = Queue [2,8,5] [4,7]
 -- would represent que queue where the first element is 2, followed by 8, 5, 7 and 4.

 {-
 In this way, the push operation is made by prepending an element to the second list (which is less expensive than appending it to its end).

On the other hand, the pop operation is now made by extracting the first element in the first queue, provided it exists. If it does not exist, all the elements in the second list are transferred to the first list (by reversing its order).

Implement generic queues using the following interface: -}
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
 