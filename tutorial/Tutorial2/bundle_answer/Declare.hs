module Declare where

{-

|---------+----+----+----+----+----+----+----+----+----|
| Problem | Q1 | Q2 | Q3 | Q4 | Q5 | Q6 | Q7 | Q8 | Q9 |
|---------+----+----+----+----+----+----+----+----+----|
| Points  | 5  | 5  | 10 | 5  | 15 | 15 | 10 | 15 | 20 |
|---------+----+----+----+----+----+----+----+----+----|

-}

data Exp = Num Int
         | Add Exp Exp
         | Sub Exp Exp
         | Mult Exp Exp
         | Div Exp Exp
         | Neg Exp
         | Power Exp Exp
         -- deriving Show

{- Question 2: The answer is Option A -}

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
showExpr (Num n) = show n
showExpr (Add exp1 exp2) = "(" ++ showExpr exp1 ++ " + " ++ showExpr exp2 ++ ")"
showExpr (Sub exp1 exp2) = "(" ++ showExpr exp1 ++ " - " ++ showExpr exp2 ++ ")"
showExpr (Mult exp1 exp2) = "(" ++ showExpr exp1 ++ " * " ++ showExpr exp2 ++ ")"
showExpr (Div exp1 exp2) = "(" ++ showExpr exp1 ++ " / " ++ showExpr exp2 ++ ")"
showExpr (Neg e) = "(-" ++ showExpr e ++ ")"
showExpr (Power exp1 exp2) = "(" ++ showExpr exp1 ++ " ^ " ++ showExpr exp2 ++ ")"
