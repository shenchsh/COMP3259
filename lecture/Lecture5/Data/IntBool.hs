module IntBool where
import Prelude hiding (LT, GT, EQ, id)
import Base
import Value
import Data.Maybe

data Exp = Literal   Value      -- integers or booleans
         | Unary     UnaryOp Exp        -- -3, not true
         | Binary    BinaryOp Exp Exp   -- 3 + 4, 3 == 4, true && false 
         | If        Exp Exp Exp        -- if (x > 0) then x else -x
         | Variable  String
         | Declare   String Exp Exp

{- TODO: If expressions -}  

evaluate :: Exp -> Env -> Value
evaluate (Literal v) env      = v
evaluate (Unary op a) env     = unary op (evaluate a env)
evaluate (Binary op a b) env  = binary op (evaluate a env) (evaluate b env)
evaluate (Variable x) env     = fromJust (lookup x env)
evaluate (Declare x exp body) env = evaluate body newEnv
  where newEnv = (x, evaluate exp env) : env
evaluate (If a b c) env = undefined

execute exp = evaluate exp []


-- Question: Design choices for if expressions.

-- Code to display expressions

instance Show Exp where
  show e = "[" ++ showExp 0 e ++ "]"

showExp level (Literal i)      = show i
showExp level (Variable x)    = x
showExp level (Declare x a b) = 
  if level > 0 then paren result else result
    where result = "var " ++ x ++ " = " ++ showExp 0 a ++ "; " ++ showExp 0 b
showExp level (If a b c)    = 
  if level > 0 then paren result else result
    where result = "if (" ++ showExp 0 a ++ ") " ++ showExp 0 b ++ "; else " ++ showExp 0 c
showExp level (Unary Neg a)    = "-" ++ showExp 99 a
showExp level (Unary Not a)    = "!" ++ showExp 99 a
showExp level (Binary op a b)  = showBinary level (fromJust (lookup op levels)) a (fromJust (lookup op names)) b
  where levels = [(Or, 1), (And, 2), (GT, 3), (LT, 3), (LE, 3), (GE, 3), (EQ, 3), 
                  (Add, 4), (Sub, 4), (Mul, 5), (Div, 5)] 
        names = [(Or, "||"), (And, "&&"), (GT, ">"), (LT, "<"), (LE, "<="), (GE, ">="), (EQ, "=="), 
                  (Add, "+"), (Sub, "-"), (Mul, "*"), (Div, "/")] 

showBinary outer inner a op b =
  if inner < outer then paren result else result
      where result = showExp inner a ++ " " ++ op ++ " " ++ showExp inner b