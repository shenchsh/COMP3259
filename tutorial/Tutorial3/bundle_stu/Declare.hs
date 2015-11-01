module Declare where

data Exp = Num Int
         | Add Exp Exp
         | Sub Exp Exp
         | Mult Exp Exp
         | Div Exp Exp
         | Var String            -- new
         | Decl String Exp Exp   -- new
         | DeclareMulti [(String, Exp)] Exp
         -- TODO: Question 4


e1 :: Exp
e1 = Decl "x" (Num 3) (Mult (Var "x") (Num 3))

e2 :: Exp
e2 = Decl "x" (Add (Num 3) (Num 4)) (Var "x")

e3 :: Exp
e3 = Add (Var "x") (Mult (Num 4) (Num 5))

e4 :: Exp
e4 = Decl "y" e3 (Div (Var "x") (Var "y"))


instance Show Exp where
  show = showExpr


-- | Pretty printer
--
-- Examples:
--
-- >>> e1
-- var x = 3; (x * 3)
--
-- >>> e2
-- var x = (3 + 4); x
--
-- >>> e3
-- (x + (4 * 5))
--
-- >>> e4
-- var y = (x + (4 * 5)); (x / y)
showExpr :: Exp -> String
showExpr (Num a) = show a
showExpr (Add e1 e2) = "(" ++ showExpr e1 ++ " + " ++ showExpr e2 ++ ")"
showExpr (Sub e1 e2) = "(" ++ showExpr e1 ++ " - " ++ showExpr e2 ++ ")"
showExpr (Mult e1 e2) = "(" ++ showExpr e1 ++ " * " ++ showExpr e2 ++ ")"
showExpr (Div e1 e2) = "(" ++ showExpr e1 ++ " / " ++ showExpr e2 ++ ")"
showExpr (Var a) = a
showExpr (Decl s e1 e2) = "var " ++ s ++ " = " ++ showExpr e1 ++ "; " ++ showExpr e2
showExpr (DeclareMulti list e1) = "var " ++ showFirst (head list) ++ showRest (tail list) ++ "; " ++ (showExpr e1) 
    where
        showFirst::(String, Exp) -> String
        showFirst (s, e) = s ++ " = " ++ showExpr e

        showRest::[(String, Exp)] -> String
        showRest [] = ""
        showRest ((s, e):xs) = ", " ++ s ++ " = " ++ showExpr e ++ showRest xs

--showExpr = error "TODO: Question 1"



-- | Renaming function
--
-- Examples:
--
-- >>> rename "x" "i" e3
-- (i + (4 * 5))
--
-- >>> rename "x" "i" e1
-- var x = 3; (x * 3)
--
-- >>> rename "x" "i" e4
-- var y = (i + (4 * 5)); (i / y)

-- e1 :: Exp
-- e1 = Decl "x" (Num 3) (Mult (Var "x") (Num 3))

rename :: String -> String -> Exp -> Exp
rename var newvar exp = rename1 exp 
    where
        rename1 (Num a) = Num a
        rename1 (Add e1 e2) = Add (rename1 e1) (rename1 e2)
        rename1 (Sub e1 e2) = Sub (rename1 e1) (rename1 e2)
        rename1 (Mult e1 e2) = Mult (rename1 e1) (rename1 e2)
        rename1 (Div e1 e2) = Div (rename1 e1) (rename1 e2)
        rename1 (Var a) = if (a == var) then (Var newvar) else (Var a)
        rename1 (Decl s e1 e2) =
            if var == s
            then Decl var (rename1 e1) e2
            else Decl var (rename1 e1) (rename1 e2)
        rename1 (DeclareMulti decls exp) =
            case decls of
                [] -> exp
                ((s, e):xs) -> rename1 (DeclareMulti xs (Decl s e exp))

--rename = error "TODO: Question 2"
