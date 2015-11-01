{- File with basic arithmetic expressions + Variables -}

module Syntax where

import Base

{- We modified the abstract syntax of expressions to add a new
case:

   Variable :: String -> Exp

which allows us to create variable expressions. Now it it possible
to write expressions such as "x + 5", which are represented in the
abstract syntax as:

Add (Variable "x") (Number 5)

-}

-- BEGIN:Abst3
data Exp = Number     Int
         | Add        Exp Exp
         | Subtract   Exp Exp
         | Multiply   Exp Exp
         | Divide     Exp Exp
         | Variable   String --added
-- END:Abst3

-- an example of an expression with variables

exp1 :: Exp
exp1 = Add (Number 5) (Variable "x") -- 5 + x














{- The code that follows can be igored for now: it provides
a function that pretty prints the output nicely.
-}

--BEGIN:Form10
instance Show Exp where
  show e = "[" ++ showExp 0 e ++ "]"

showExp :: Int -> Exp -> String
showExp level (Number i)      = show i
showExp level (Variable x)    = x
showExp level (Add a b)       = showBinary level 1 a " + " b
showExp level (Subtract a b)  = showBinary level 1 a " - " b
showExp level (Multiply a b)  = showBinary level 2 a "*" b
showExp level (Divide a b)    = showBinary level 2 a "/" b

showBinary :: Int -> Int -> Exp -> String -> Exp -> String
showBinary outer inner a op b =
  if inner < outer then paren result else result
      where result = showExp inner a ++ op ++ showExp inner b
--END:Form10
