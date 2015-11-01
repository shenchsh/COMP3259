module Declare where

data Exp = Num Int
         | Add Exp Exp
         | Sub Exp Exp
         | Mult Exp Exp
         | Div Exp Exp
         | Var String            -- new
         | Decl String Exp Exp   -- new
         | DeclareMulti [(String, Exp)] Exp


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
showExpr (Num n) = show n
showExpr (Add exp1 exp2) = "(" ++ showExpr exp1 ++ " + " ++ showExpr exp2 ++ ")"
showExpr (Sub exp1 exp2) = "(" ++ showExpr exp1 ++ " - " ++ showExpr exp2 ++ ")"
showExpr (Mult exp1 exp2) = "(" ++ showExpr exp1 ++ " * " ++ showExpr exp2 ++ ")"
showExpr (Div exp1 exp2) = "(" ++ showExpr exp1 ++ " / " ++ showExpr exp2 ++ ")"
showExpr (Var x) = x
showExpr (Decl v b e) = "var " ++ v ++ " = " ++ showExpr b ++ "; " ++ showExpr e
showExpr (DeclareMulti env body) = showEnv "var " "" env ++ showExpr body
  where
    showEnv _ end [] = end
    showEnv start _ ((x, e):xs) = start ++ x ++ " = " ++ showExpr e ++ showEnv ", " "; " xs



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
rename :: String -> String -> Exp -> Exp
rename name rname e = re e
  where
    re (Num x) = Num x
    re (Add x y) = Add (re x) (re y)
    re (Sub x y) = Sub (re x) (re y)
    re (Mult x y) = Mult (re x) (re y)
    re (Div x y) = Div (re x) (re y)
    re (Var x) = if x == name
                   then Var rname
                   else Var x
    re (Decl n ex1 ex2) = if name == n
                            then Decl n (re ex1) ex2
                            else Decl n (re ex1) (re ex2)
    re (DeclareMulti env body) =
      case lookup name env of
        Just _  -> DeclareMulti (map (\(v, ex) -> (v, re ex)) env) body
        Nothing -> DeclareMulti (map (\(v, ex) -> (v, re ex)) env) (re body)
