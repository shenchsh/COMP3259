module TypeCheck where

import Declare
import Interp
import Parser
import Prelude hiding (LT, GT, EQ)

data Type
  = TInt
  | TBool
  deriving (Eq,Show)

type TEnv = [(String,Type)]

-- | Question 4
--
-- >>> tunary Neg TInt
-- Just TInt
--
-- >>> tbinary Add TInt TBool
-- Nothing
tunary :: UnaryOp -> Type -> Maybe Type
tunary a b =
    case (a, b) of
        (Neg, TInt) ->  Just TInt
        (Neg, TBool) -> Nothing
        (Not, TInt) -> Nothing
        (Not, TBool) -> Just TBool
-- tunary = error "TODO: Question 4"

tbinary :: BinaryOp -> Type -> Type -> Maybe Type
tbinary a b c 
    | a `elem` [Add, Sub, Mult, Div, Power] = case (b, c) of
                                                (TInt, TInt) -> Just TInt
                                                (_, _) -> Nothing
    | a `elem` [GT, LT, LE, GE, EQ] = case (b, c) of
                                        (TBool, TBool) ->Just TBool
                                        (TInt, TInt) -> Just TBool
                                        (_, _) -> Nothing
    | a `elem` [And, Or] = case (b, c) of
                            (TBool, TBool) -> Just TBool
                            (_, _) -> Nothing
-- tbinary = error "TODO: Question 4"


-- | Question 5
--
-- >>> testq4 "1"
-- Just TInt
--
-- >>> testq4 "false"
-- Just TBool
--
-- >>> testq4 "1*false"
-- Nothing
--
-- >>> testq4 "var x = 5; if (x > 0) x; else x * x"
-- Just TInt
--
-- >>> testq4 "var x = y; var y = 3; x + y"
-- Nothing
tcheck :: Exp -> TEnv -> Maybe Type
tcheck (Literal a) env =
    case a of
        IntV _ -> Just TInt
        BoolV _ -> Just TBool

tcheck (Unary op e) env =
    case (op, e1) of
        (Neg, Just TInt) -> Just TInt
        (Neg, _) -> Nothing
        (Not, Just TBool) -> Just TBool
        (Not, _) -> Nothing
    where e1 = tcheck e env

tcheck (Binary op e1 e2) env
    | op `elem` [Add, Sub, Mult, Div, Power] = case (tcheck e1 env, tcheck e2 env) of
                                                (Just TInt, Just TInt) -> Just TInt
                                                (_, _) -> Nothing
    | op `elem` [GT, LT, LE, GE, EQ] = case (tcheck e1 env, tcheck e2 env) of
                                            (Just TBool, Just TBool) -> Just TBool
                                            (Just TInt, Just TInt) -> Just TBool
                                            (_, _) -> Nothing
    | op `elem` [And, Or] = case (tcheck e1 env, tcheck e2 env) of
                                            (Just TBool, Just TBool) -> Just TBool
                                            (_, _) -> Nothing

tcheck (If e1 e2 e3) env =
    case (tcheck e1 env)  of
        Just TBool -> case (tcheck e2 env, tcheck e3 env) of
                        (Just TInt, Just TInt) -> Just TInt
                        (Just TBool, Just TBool) -> Just TBool
                        (_, _) -> Nothing
        _ -> Nothing

tcheck (Var s) env =
    case lookup s env of
        Just a -> Just a
        Nothing -> Nothing

tcheck (Decl s e1 e2) env =
    case (tcheck e1 env) of
        Just TInt -> case (tcheck e2 newEnv) of
                        Just TInt -> Just TInt
                        Just TBool -> Just TBool
                        _ -> Nothing
                        where newEnv = (s, TInt):env
        Just TBool -> case (tcheck e2 newEnv) of
                        Just TInt -> Just TInt
                        Just TBool -> Just TBool
                        _ -> Nothing
                        where newEnv = (s, TBool):env
        Nothing -> Nothing

-- tcheck = error "TODO: Question 4"

-- | Question 6
--
-- >>> tcalc "3 == 3"
-- true
--
-- >>> tcalc "if (3 == 4) true; else false"
-- false
--
-- >>> tcalc "var x = 3; x + true"
-- *** Exception: You have a type-error in your program!
--
-- >>> tcalc "var x = 4; var y = (x > 5); if (y == true || y < true) 1; else 0"
-- 1
--
-- >>> tcalc "var x = 8; var y = (var z = 4; if (z > 3 && z < 5) x; else x*x); y"
-- 8
tcalc :: String -> Value
tcalc s = 
    case testq4 s of
        Just a -> calc s
        Nothing -> error"You have a type-error in your program!"
        
-- tcalc = error "TODO: Question 5"

testq4 :: String -> Maybe Type
testq4 e = tcheck (parseExpr e) []
