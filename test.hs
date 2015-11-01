bad :: [a] -> Int
bad [] = 0
bad (x:xs) = 1 + bad xs

addOne :: [Int] -> [Int]
addOne [] = []
addOne (x:xs) = (x + 1):addOne xs

data ListInt = Nil | Cons Int ListInt deriving (Eq, Show)

doubleList :: ListInt -> ListInt
doubleList Nil = Nil
doubleList (Cons x xs) = Cons (x*2) (doubleList xs)

elem' :: (Eq a) => a -> [a] -> Bool
elem' y ys = foldl (\acc x -> if x == y then True else acc) False ys

data Tree = Value Int | Branch Tree Tree deriving (Eq, Show) 
atree = Branch (Value 2) (Value 3)

collect (Value a) = [a]
collect (Branch t1 t2) = (collect t1) ++ (collect t2)

data Exp = Number Int
    |Add Exp Exp
    |Minus Exp Exp
    |Multiple Exp Exp
    |Divide Exp Exp deriving (Eq, Show)

evaluate :: Exp -> Int
evaluate (Number a) = a
evaluate (Add e1 e2) = (evaluate e1) + (evaluate e2)
evaluate (Minus e1 e2) = (evaluate e1) - (evaluate e2)
evaluate (Multiple e1 e2) = (evaluate e1) * (evaluate e2)
evaluate (Divide e1 e2) =
    let v = evaluate e2
    in 
        if (v == 0) then 
            error "Division by 0"
        else div (evaluate e1) v

e1 :: Exp
e1 = Divide (Number 1) (Number 0)
