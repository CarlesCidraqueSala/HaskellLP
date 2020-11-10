{- We want to have a module to mainpulate and evaluate integer expressions with addition, substraction, multiplication and division operations.
 In order to do so, the following type is declared: -}

data Expr = Val Int | Add Expr Expr | Sub Expr Expr | Mul Expr Expr | Div Expr Expr

--For instance, Add (Val 3) (Div (Val 4) (Val 2)) represents 3 + 4 / 2, which evaluates to 5.

 --Using the Expr type, define a function eval1 :: Expr -> Int that, given an expression, returns its evaluation. You can assume there will never be divisions by zero.

eval1 :: Expr -> Int
eval1 (Val x) = x
eval1 (Add expr1 expr2) = (eval1 expr1) + (eval1 expr2)
eval1 (Sub expr1 expr2) = (eval1 expr1) - (eval1 expr2)
eval1 (Mul expr1 expr2) = (eval1 expr1) * (eval1 expr2)
eval1 (Div expr1 expr2) = div (eval1 expr1) (eval1 expr2)

{-
Using the Expr type, define a function eval2 :: Expr -> Maybe Int that, given an expression, returns its evaluationn as a Just value. 
In the case that some division by zero occurs, the result must be Nothing. You probably want to use the do notation over the Maybe a monad.
-}
eval2 :: Expr -> Maybe Int
eval2 (Val x) = return x
eval2 (Add x y) = eval2' (+) x y
eval2 (Sub x y) = eval2' (-) x y
eval2 (Mul x y) = eval2' (*) x y
eval2 (Div x y) = eval2div x y

safe2Div :: Int -> Int -> Maybe Int
safe2Div n m
    | m == 0 = Nothing
    | otherwise = Just (div n m)

eval2div :: Expr -> Expr -> Maybe Int
eval2div x y = do i <- eval2 x
                  j <- eval2 y
                  safe2Div i j

eval2' :: (Int -> Int -> Int) -> Expr -> Expr -> Maybe Int
eval2' f x y = do i <- eval2 x
                  j <- eval2 y
                  Just (f i j)

{-
Using the Expr type, define a function eval3 :: Expr -> Either String Int that, given an expression, returns its evaluation as Right value. 
In the case that some division by zero occurs, the result must be Left "div0". 
You probably want to use the do notation over the Either a b monad.
-}
eval3 :: Expr -> Either String Int
eval3 (Val x) = Right x
eval3 (Add x y) = eval3' (+) x y
eval3 (Sub x y) = eval3' (-) x y
eval3 (Mul x y) = eval3' (*) x y
eval3 (Div x y) = eval3div x y

safe3Div :: Int -> Int -> Either String Int
safe3Div n m
    | m == 0    = Left "div0"
    | otherwise = Right (n `div` m)

eval3div :: Expr -> Expr -> Either String Int
eval3div x y = do i <- eval3 x
                  j <- eval3 y
                  safe3Div i j

eval3' :: (Int -> Int -> Int) -> Expr -> Expr -> Either String Int
eval3' f x y = do i <- eval3 x
                  j <- eval3 y
                  Right (f i j)