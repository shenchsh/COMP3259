module Substitution where

import Syntax
import SubstituteParse

{- The following type, describes the type of a binding:
a binding is just a pair of a variable and a value.

The "type" keyword introduces an alias for some algebraic data type

-}

type Binding = (String,Int)

{- Substituion is the operation that takes a binding
(or a variable name and a value) and an expression and
replaces all occurrences of the bound variable in the expression
by the corresponding value. For example:

*Simple> substitute1 ("x",4) exp -- exp is the expression "5 + x"
[5 + 4]

-}

{-
data Exp = Number     Int
         | Add        Exp Exp
         | Subtract   Exp Exp
         | Multiply   Exp Exp
         | Divide     Exp Exp
         | Variable   String --added

-}

substitute1 :: Binding -> Exp -> Exp
substitute1 (name,value) (Variable x) =
  if (x == name) then Number value else Variable x
substitute1 b (Add e1 e2) =
  Add (substitute1 b e1) (substitute1 b e2)
substitute1 b (Subtract e1 e2) =
  Subtract (substitute1 b e1) (substitute1 b e2)
substitute1 b (Multiply e1 e2) =
  Multiply (substitute1 b e1) (substitute1 b e2)
substitute1 b (Divide e1 e2) =
  Divide (substitute1 b e1) (substitute1 b e2)
substitute1 b (Number x) = Number x

-- A simple binding

bind1 :: Binding
bind1 = ("x", 5)

-- "x + 2"
e1 :: Exp
e1 = Add (Variable "x") (Number 2)

-- "32"
e2 :: Exp
e2 = Number 32

-- "x"
e3 :: Exp
e3 = Variable "x"

-- "x * x + x"
e4 :: Exp
e4 = Add (Multiply (Variable "x") (Variable "x")) (Variable "x")

-- "x + 2 * y + z"
e5 :: Exp
e5 = Add (Variable "x") (Add (Multiply (Number 2) (Variable "y")) (Variable "z"))













{- Environments: An environment is a collection of bindings: -}

type Env = [Binding] -- [(String,Int)]

{- Multiple substitution:

Multiple substitution allows substituting multiple variables
in an expression. Unlike "substitute1" which takes a single
binding, multiple substitution takes an environent (or a
collection of bindings) and uses that to lookup the values
of variables.
-}

-- lookup :: String -> [(String,Int)] -> Maybe Int

substitute :: Env -> Exp -> Exp
substitute env (Number n) = Number n
substitute env (Variable v) =
  case lookup v env of
     Nothing -> Variable v
     Just y -> Number y
substitute b (Add e1 e2) =
  Add (substitute b e1) (substitute b e2)
substitute b (Subtract e1 e2) =
  Subtract (substitute b e1) (substitute b e2)
substitute b (Multiply e1 e2) =
  Multiply (substitute b e1) (substitute b e2)
substitute b (Divide e1 e2) =
  Divide (substitute b e1) (substitute b e2)













substituteExp :: Env -> String -> Exp
substituteExp env e = substitute env (parseExp e)

-- A sample environment

env1 :: Env
env1 = [("x",6),("y",5)]

{- Tests: try these in ghci -}

test1 :: Exp
test1 = substituteExp env1 "5"

test2 :: Exp
test2 = substituteExp env1 "x + x"

test3 :: Exp
test3 = substituteExp env1 "x + y"

test4 :: Exp
test4 = substituteExp env1 "x + y * z"









{- Evaluation using an environment:

To deal with variables, evaluation can be modified to
take an environment. Using that environment it becomes
possible to lookup the value of variables and deal
with the new Variable case.

-}

evaluate :: Env -> Exp -> Int
evaluate env (Number x) = x
evaluate env (Add e1 e2) = evaluate env e1 + evaluate env e2
evaluate env (Subtract e1 e2) = evaluate env e1 - evaluate env e2
evaluate env (Multiply e1 e2) = evaluate env e1 * evaluate env e2
evaluate env (Divide e1 e2) =
  case evaluate env e2 of
     0 -> error "Division by 0!"
     n -> div (evaluate env e1) n
evaluate env (Variable name) =
  case lookup name env of
     Nothing -> error ("Undefined variable " ++ name ++ "!")
     Just v -> v

