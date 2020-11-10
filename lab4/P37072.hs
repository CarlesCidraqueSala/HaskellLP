-- In this problem you have to write several functions for generic binary trees. The definition of the trees is given by:

data Tree a = Node a (Tree a) (Tree a) | Empty deriving (Show)

-- Write a function size :: Tree a -> Int that, given a tree, returns its size, that is, the number of node it contains.

size :: Tree a -> Int 

size Empty = 0
size (Node _ fe fd) = 1 + size fe + size fd

-- Write a function height :: Tree a -> Int that, given a tree, returns its height, assuming that empty trees have zero height.

height :: Tree a -> Int
 
height Empty = 0
height (Node _ fe fd) = 1 + max (height fe) (height fd)

-- Write a function equal :: Eq a => Tree a -> Tree a -> Bool that, given two trees, tells whether they are the same.

equal :: Eq a => Tree a -> Tree a -> Bool

equal Empty (Node _ _ _) = False
equal (Node _ _ _) Empty = False
equal Empty Empty = True
equal (Node x fe1 fd1) (Node y fe2 fd2) = (x == y) && equal fe1 fe2 && equal fd1 fd2

-- Write a function isomorphic :: Eq a => Tree a -> Tree a -> Bool that, given two trees,
--  tells whether they are isomorphic, that is, if one can obtain one from the other flipping some of its descendants.

isomorphic :: Eq a => Tree a -> Tree a -> Bool
isomorphic Empty (Node _ _ _) = False
isomorphic (Node _ _ _) Empty = False
isomorphic Empty Empty = True
isomorphic (Node x1 a1 b1) (Node x2 a2 b2) = x1 == x2 &&
                                           ((equal a1 a2 &&
                                             equal b1 b2) ||
                                            (equal a1 b2 &&
                                             equal b1 a2))

--Write a function preOrder :: Tree a -> [a] that, given a tree, return its pre-order traversal.

preOrder :: Tree a -> [a]

preOrder Empty = []
preOrder (Node x fe fd) = [x] ++ preOrder fe ++ preOrder fd

-- Write a function postOrder :: Tree a -> [a] that, given a tree, return its post-order traversal.

postOrder :: Tree a -> [a]

postOrder Empty = []
postOrder (Node x fe fd) = postOrder fe ++ postOrder fd ++ [x]

--Write a function inOrder :: Tree a -> [a] that, given a tree, return its in-order traversal.

inOrder :: Tree a -> [a]

inOrder Empty = []
inOrder (Node x fe fd) = inOrder fe ++ [x] ++ inOrder fd

--Write a function breadthFirst :: Tree a -> [a] that, given a tree, return its traversal by levels.

breadthFirst :: Tree a -> [a]
breadthFirst x = breadthFirstRec [x]

breadthFirstRec :: [Tree a] -> [a]
breadthFirstRec [] = []
breadthFirstRec (Empty:ts) = breadthFirstRec ts
breadthFirstRec ((Node x left right):ts) = x : (breadthFirstRec $ ts ++ [left, right])

--Write a function build :: Eq a => [a] -> [a] -> Tree a that, given a pre-order traversal of a tree and an in-order traversal of the same tree, 
--returns the original tree. You can assume that the three has no repeated elements.

build :: Eq a => [a] -> [a] -> Tree a
build [ ] [ ] = Empty
build (x:preorder) inorder = Node x (build leftPreorder  leftInorder ) (build rightPreorder rightInorder)
    where  
        leftInorder   = takeWhile (/= x) inorder
        leftPreorder  = take (length leftInorder) preorder
        rightPreorder = drop (length leftInorder) preorder
        rightInorder  = tail (dropWhile (/= x) inorder)

--Write a function overlap :: (a -> a -> a) -> Tree a -> Tree a -> Tree a that, given two trees, returns its overlapping using a function. 
--Overlapping two trees with a function consists in placing the two trees one on the other and combine the double nodes using the given function.

overlap :: (a -> a -> a) -> Tree a -> Tree a -> Tree a
overlap _ tree1 Empty = tree1
overlap _ Empty tree2 = tree2
overlap op (Node x1 a1 b1) (Node x2 a2 b2) = Node (foldl op x1 [x2])
                                             (overlap op a1 a2)
                                             (overlap op b1 b2)