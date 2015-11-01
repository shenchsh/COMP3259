module Declare where

{-

|---------+----+----+----+----+----+----+----+----|
| Problem | Q1 | Q2 | Q3 | Q4 | Q5 | Q6 | Q7 | Q8 |
|---------+----+----+----+----+----+----+----+----|
| Points  | 5  | 5  | 10 | 5  | 15 | 20 | 15 | 25 |
|---------+----+----+----+----+----+----+----+----|

-}

data Exp = Num Int
         | Add Exp Exp
         | Sub Exp Exp
         | Mult Exp Exp
         | Div Exp Exp
         -- TODO: Question 1
         -- Add a unary operator "Neg" and a binary operator "Power"
         | Neg Exp
         | Power Exp Exp
         --deriving Show

-- TODO: Please write your answer to Question 2 below
-- Option A


e1 :: Exp
e1 = Add (Num 3) (Num 4)

e2 :: Exp
e2 = Add (Num 3) (Mult (Sub (Num 4) (Num 5)) (Num 7))

e3 :: Exp
e3 = Sub (Div (Add (Num 1) (Num 2)) (Num 3)) (Mult (Sub (Num 5) (Num 6)) (Num 8))


instance Show Exp where
    show = showExpr

-- | Printer printer
--
-- Examples:
--
-- >>> e1
-- (3 + 4)
--
-- >>> e2
-- (3 + ((4 - 5) * 7))
--
-- >>> e3
-- (((1 + 2) / 3) - ((5 - 6) * 8))
showExpr :: Exp -> String
showExpr e =
    case e of
        Num a -> show a
        Add a b -> "(" ++ (showExpr a) ++ " + " ++  (showExpr b) ++ ")"
        Sub a b -> "(" ++ (showExpr a) ++ " - " ++ (showExpr b) ++ ")"
        Mult a b -> "(" ++ (showExpr a) ++ " * " ++ (showExpr b) ++ ")" 
        Div a b -> "(" ++ (showExpr a) ++ " / " ++ (showExpr b) ++ ")"
        Neg a -> "(-" ++ (showExpr a) ++ ")"
        Power a b -> "(" ++ (showExpr a) ++ "^" ++ (showExpr b) ++ ")"
-- showExpr = error "TODO: Question 5"
