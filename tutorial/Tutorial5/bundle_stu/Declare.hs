module Declare where

import Data.Maybe (fromJust)
import Prelude hiding (LT, GT, EQ)

data BinaryOp = Add | Sub | Mult | Div | Power
              | And | Or  | GT   | LT  | LE
              | GE  | EQ
              deriving (Show, Eq)

data UnaryOp
  = Neg
  | Not
  deriving (Show,Eq)

data Value
  = IntV Int    -- Integers
  | BoolV Bool  -- Boolean values
  deriving (Eq)

data Type
  = TInt
  | TBool
  deriving (Eq)

instance Show Value where
  show (IntV n) = show n
  show (BoolV True) = "true"
  show (BoolV False) = "false"

instance Show Type where
  show TInt = "Int"
  show TBool = "Bool"



data Program = Program FunEnv Exp

type FunEnv = [(String, Function)]

data Function = Function [(String, Type)] Exp

data Exp = Lit Value
         | Unary UnaryOp Exp
         | Bin BinaryOp Exp Exp
         | If Exp Exp Exp
         | Var String
         | Decl String Exp Exp
         | Call String [Exp]

prog1 :: Program
prog1 =
  Program
    [ ("absolute", Function [("x", TInt)]
                     (If (Bin GT (Var "x") (Lit (IntV 0))) (Var "x") (Unary Neg (Var "x"))))
    , ("max", Function [("x", TInt), ("y", TInt)]
                (If (Bin GT (Var "x") (Var "y")) (Var "x") (Var "y")))
    ]
    (Call "max" [Call "absolute" [Lit (IntV (-5))], Lit (IntV 4)])

prog2 :: Program
prog2 =
    Program [("fact", Function [("x", TInt)] (If (Bin EQ (Var "x") (Lit (IntV 0))) (Lit (IntV 1)) (Bin Mult (Var "x") (Call "fact" [Bin Sub (Var "x") (Lit (IntV 1))])))
        )] (Call "fact" [Lit (IntV (5))])

prog3 :: Program
prog3 =
    Program [("fact", Function [("x", TInt)] (If (Bin EQ (Var "x") (Lit (IntV 0))) (Lit (IntV 1)) (Bin Mult (Var "x") (Bin Sub (Var "x") (Lit (IntV 1)))))
        )] (Lit (IntV (5)))
-- Pretty printer

prog4 :: Program
prog4 =
    Program [("fact", Function [("x", TInt)] (If (Bin EQ (Var "x") (Lit (IntV 0))) (Lit (IntV 1)) (Bin Mult (Var "x") (Call "fact" [Bin Sub (Var "x") (Lit (IntV 1))])))
        )] (Lit (IntV (5)))

instance Show Program where
    show (Program fenv e) = -- error "TODO: Question 4"
        showSep "\n" (map showFun fenv) ++ "\n" ++  show e 

-- | Question 3
--
-- showFun ("foo",Function [("x",TInt),("y",TInt)] (Bin Add (Var "x") (Var "y")))
-- function foo(x : Int, y : Int) {
-- x + y
-- }
showFun :: (String, Function) -> String
showFun (name, Function args body) =
    "function " ++ name ++ "(" ++ (showSep ", " (map (\(s, t) -> s ++ " : " ++ show t) args))  ++ ") {\n "  ++ show body ++ "\n}"
-- showFun (name, Function args body)  = error "TODO: Question 3"


-- | Question 1
--
-- >>> showSep "; " ["hello", "to", "world"]
-- "hello; to; world"
--
showSep :: String -> [String] -> String
showSep _ [] = ""
showSep _ [x] = x
showSep s (x:xs) = (x ++ s) ++  showSep s xs
-- showSep = error "TODO: Question 1"


instance Show Exp where
  show = showExp 0

-- | Question 2
-- 
-- >>> show (Call "max" [(Lit (IntV 3)), (Lit (IntV 4))])
-- "max(3, 4)"
--
showExp :: Int -> Exp -> String
showExp _ (Call f args) = --error "TODO: Question 2"
    f ++ "(" ++ (showSep ", " (map show args)) ++ ")"
showExp _ (Lit i) = show i
showExp _ (Var x) = x
showExp level (Decl x a b) =
  if level > 0
    then paren result
    else result
  where
    result = "var " ++ x ++ " = " ++ showExp 0 a ++ "; " ++ showExp 0 b
showExp level (If a b c) =
  if level > 0
    then paren result
    else result
  where
    result = "if (" ++ showExp 0 a ++ ") " ++ showExp 0 b ++ "; else " ++ showExp 0 c
showExp _ (Unary Neg a) = "-" ++ showExp 99 a
showExp _ (Unary Not a) = "!" ++ showExp 99 a
showExp level (Bin op a b) =
  showBin level (fromJust (lookup op levels)) a (fromJust (lookup op names)) b
  where
    levels =
      [ (Or, 1)
      , (And, 2)
      , (GT, 3)
      , (LT, 3)
      , (LE, 3)
      , (GE, 3)
      , (EQ, 3)
      , (Add, 4)
      , (Sub, 4)
      , (Mult, 5)
      , (Div, 5)
      , (Power, 6)
      ]
    names =
      [ (Or, "||")
      , (And, "&&")
      , (GT, ">")
      , (LT, "<")
      , (LE, "<=")
      , (GE, ">=")
      , (EQ, "==")
      , (Add, "+")
      , (Sub, "-")
      , (Mult, "*")
      , (Div, "/")
      , (Power, "^")
      ]

showBin :: Int -> Int -> Exp -> String -> Exp -> String
showBin outer inner a op b =
  if inner < outer
    then paren result
    else result
  where
    result = showExp inner a ++ " " ++ op ++ " " ++ showExp inner b

paren :: String -> String
paren x = "(" ++ x ++ ")"
