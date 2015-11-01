module Interp where

import Parser
import Declare
import Data.List (delete, union, sort)
import Prelude hiding (LT, GT, EQ)


-- | Question 1
--
-- >>> testq1 "1 + 2"
-- []
--
-- >>> testq1 "x * x"
-- ["x"]
--
-- >>> testq1 "var x = 3; x * y * z"
-- ["y","z"]
--
-- >>> testq1 "var x = y; var y = 3; x + y"
-- ["y"]
--
-- >>> testq1 "var x = x + y; var y = 4; x + z"
-- ["x","y","z"]
--

fv :: Exp -> [String]
fv (Literal a) = []
fv (Unary uOp e1) = fv e1
fv (Binary bOp e1 e2) = union (fv e1) (fv e2)
fv (If e1 e2 e3) = union (union (fv e1) (fv e2)) (fv e3)
fv (Var s) = [s]
fv (Decl s e1 e2) = union (fv e1) (delete s (fv e2))
-- fv = error "TODO: Question 1"


-- | Question 2
--
-- >>> unary Not (BoolV True)
-- false
--
-- >>> unary Neg (IntV 3)
-- -3
--
-- >>> binary Add (IntV 2) (IntV 3)
-- 5
unary :: UnaryOp -> Value -> Value
unary Neg a =
    case a of
        BoolV b -> error"undefined"
        IntV b -> IntV (negate b)

unary Not a =
    case a of
        IntV b -> error"undefined"
        BoolV True -> BoolV False
        BoolV False -> BoolV True
-- unary = error "TODO: Question 2"

binary :: BinaryOp -> Value -> Value -> Value
binary Add a b =
    case (a, b) of
        (IntV c, IntV d) -> IntV (c + d)
        (_, _) -> error"undefined"
binary Sub a b =
    case (a, b) of
        (IntV c, IntV d) -> IntV (c - d)
        (_, _) -> error"undefined"
binary Mult a b =
    case (a, b) of
        (IntV c, IntV d) -> IntV (c * d)
        (_, _) -> error"undefined"
binary Div a b =
    case (a, b) of
        (IntV c, IntV 0) -> error"Divided by zero"
        (IntV c, IntV d) -> IntV (c `div` d)
        (_, _) -> error"undefined"
binary Power a b =
    case (a, b) of
        (IntV c, IntV d) -> IntV (c ^ d)
        (_, _) -> error"undefined"
binary And a b =
    case (a, b) of
        (BoolV c, BoolV d) -> BoolV (c && d)
        (_, _) -> error"undefined"
binary Or a b =
    case (a, b) of
        (BoolV c, BoolV d) -> BoolV (c || d)
        (_, _) -> error"undefined"
binary GT a b =
    case (a, b) of
        (IntV c, IntV d) -> BoolV (c > d)
        (BoolV c, BoolV d) -> BoolV (c > d)
        (_, _) -> error"undefined"
binary LT a b =
    case (a, b) of
        (IntV c, IntV d) -> BoolV (c < d)
        (BoolV c, BoolV d) -> BoolV (c < d)
        (_, _) -> error"undefined"
binary LE a b =
    case (a, b) of
        (IntV c, IntV d) -> BoolV (c <= d)
        (BoolV c, BoolV d) -> BoolV (c <= d)
        (_, _) -> error"undefined"
binary GE a b =
    case (a, b) of
        (IntV c, IntV d) -> BoolV (c >= d)
        (BoolV c, BoolV d) -> BoolV (c >= d)
        (_, _) -> error"undefined"
binary EQ a b =
    case (a, b) of
        (IntV c, IntV d) -> BoolV (c == d)
        (BoolV c, BoolV d) -> BoolV (c == d)
        (_, _) -> error"undefined"
              
-- binary = error "TODO: Question 2"


type Binding = (String, Value)
type Env = [Binding]


-- | Question 3
--
-- >>> calc "1 + 2"
-- 3
--
-- >>> calc "if (true) 1; else 3"
-- 1
--
-- >>> calc "var x = 5; if (x > 0) x; else x * x"
-- 5
--

evaluate :: Exp -> Value
evaluate e = eval e [] -- starts with an empty environment
    where
        eval :: Exp -> Env -> Value
        eval (Literal a) env = a
        eval (Unary op e1) env = unary op (eval e1 env)
        eval (Binary op e1 e2) env = binary op (eval e1 env) (eval e2 env)
        eval (If e1 e2 e3) env =
            let BoolV test = eval e1 env
            in
                if test
                then eval e2 env
                else eval e3 env
        eval (Var s) env = 
            case (lookup s env) of
                Just a -> a
                Nothing -> error "variable not in scope"

        eval (Decl s e1 e2) env = eval e2 newEnv
            where newEnv = (s, eval e1 env):env
        -- eval = error "TODO: Question 3"

calc :: String -> Value
calc = evaluate . parseExpr

testq1 :: String -> [String]
testq1 = sort . fv . parseExpr
