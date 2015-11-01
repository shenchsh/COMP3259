module Interp where

import Parser
import Declare
import Data.List (nub)

type Binding = (String, Int)
type Env = [Binding]

-- var a = 3; var b = 8; var a = b, b = a; a + b
e6 :: Exp
e6 = Decl "a" (Num 3) (Decl "b" (Num 8) (DeclareMulti [("a",(Var "b")), ("b",(Var "a"))] (Add (Var "a") (Var "b"))))

-- var a = 3; var b = 8; var a = b; var b = a; a + b
e7 :: Exp
e7 = Decl "a" (Num 3) (Decl "b" (Num 8) (Decl "a" (Var "b") (Decl "b" (Var "a") (Add (Var "a") (Var "b")))))


-- | Evaluation function
--
-- Examples:
--
-- >>> evaluate e6
-- 11
--
-- >>> evaluate e7
-- 16
evaluate :: Exp -> Int
evaluate e = eval e [] -- starts with an empty environment
  where
    eval :: Exp -> Env -> Int
    eval (Num n) _ = n
    eval (Add a b) env = eval a env + eval b env
    eval (Sub a b) env = eval a env - eval b env
    eval (Mult a b) env = eval a env * eval b env
    eval (Div a b) env = eval a env `div` eval b env
    eval (Var x) env =
      case lookup x env of
        Just v  -> v
        Nothing -> error "Not found"
    eval (Decl v a b) env =
      let a' = eval a env
          env' = (v, a') : env
      in eval b env'
    eval (DeclareMulti [] body) env = eval body env
    eval (DeclareMulti es body) env =
      if hasDuplicate es
        then error "Duplicate bindings. Please check again."
        else let env' = map (\(name, ex) -> (name, eval ex env)) es
                 nenv = env' ++ env
             in eval body nenv


hasDuplicate :: Eq a => [(a, b)] -> Bool
hasDuplicate lst = str /= nub str
  where
    str = map fst lst

calc :: String -> Int
calc  = evaluate . parseExpr



e8 :: Exp
e8 = DeclareMulti [("x", (Num 3)), ("x", (Add (Var "x") (Num 2)))] (Mult (Var "x") (Num 2))
{-

test cases for multiple declare

-- var x = 3, x = x + 2; x * 2   duplicated declaration, signal an error
e8 :: Exp
e8 = DeclareMulti [("x", (Num 3)), ("x", (Add (Var "x") (Num 2)))] (Mult (Var "x") (Num 2))

-- var a = 2, b = 7; var m = 5 * a, n = b - 1; a * n + b / m + a   evaluate to 14
e9 :: Exp
e9 = DeclareMulti [("a", (Num 2)), ("b", (Num 7))] (Add (DeclareMulti [("m", (Mult (Num 5) (Var "a"))), ("n", (Sub (Var "b") (Num 1)))] (Add (Mult (Var "a") (Var "n")) (Div (Var "b") (Var "m")))) (Var "a"))

-}
