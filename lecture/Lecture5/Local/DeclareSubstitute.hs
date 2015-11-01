module DeclareSubstitute where

import Declare hiding (evaluate, Env)
import DeclareParse

type Binding = (String,Int)



-- substitute1 ("x",5) (Number 3)  ==> Number 3

-- substitute1 ("x",5) [3+4]  ==> 3+4
-- substitute1 ("x",5) (Add (Number 3) (Number 4))  ==> Add (Number 3) (Number 4) 

-- substitute1 ("x",5) [y]  ==> y
-- substitute1 ("x",5) (Add (Variable "x") (Variable "y"))  ==> Add (Number 5) (Variable "y") 

{- Extending substitute1 with declarations -}

-- substitute1 ("x",5) [x+x]  ==> 5+5

-- substitute1 ("x",5) [var y = 2 + 3; x + y] ==>
--  var y = 2 + 3; 5 + y

substitute1 :: (String,Int) -> Exp -> Exp
substitute1 (var, val) exp = subst exp where
  subst (Number i)               = Number i
  subst (Add a b)                = Add (subst a) (subst b)
  subst (Subtract a b)           = Subtract (subst a) (subst b)
  subst (Multiply a b)           = Multiply (subst a) (subst b)
  subst (Divide a b)             = Divide (subst a) (subst b)
  subst (Variable name)          =
     if (name == var) then Number val
                      else Variable name
  subst (Declare name exp body)  =
     Declare name (subst exp)
                  (if (name == var) then body else subst body)

{- Evaluator: substitution-based -}

-- evaluate [var x = 5; x+x]  ==> 10
-- evaluate [var y = 2 + 3; 4 + y] ==> 9

-- evaluate [x + x]

evaluate :: Exp -> Int
evaluate (Number i)               = i
evaluate (Add a b)                = evaluate a + evaluate b
evaluate (Subtract a b)           = evaluate a - evaluate b
evaluate (Multiply a b)           = evaluate a * evaluate b
evaluate (Divide a b)             = evaluate a `div` evaluate b
evaluate (Declare name exp body)  = -- var x = e1; e2
  evaluate (substitute1 (name,evaluate exp) body) 
evaluate (Variable name)          = error "Unbound variable!"



{- Evaluator: environment-based -}

type Env = [(String,Int)]

-- evaluate [var x = 5; x+x]  ==> 10
-- evaluate [var y = 2 + 3; 4 + y] ==> 9

evaluateE :: Exp -> Env -> Int
evaluateE (Number i)      env  = i
evaluateE (Add a b)       env  = evaluateE a env + evaluateE b env
evaluateE (Subtract a b)  env  = evaluateE a env - evaluateE b env
evaluateE (Multiply a b)  env  = evaluateE a env * evaluateE b env
evaluateE (Divide a b)    env  = evaluateE a env `div` evaluateE b env
evaluateE (Variable name) env  =
  case lookup name env of
     Nothing -> error "Variable Unbound!"
     Just v -> v
evaluateE (Declare name a b) env =
  evaluateE b ((name,evaluateE a env):env)

e0 :: Exp
e0 = Number 3

e1 :: Exp
e1 = parseExp "5+3"

e2 :: Exp 
e2 = parseExp "x+y"

e3 :: Exp
e3 = 
  parseExp "var x = 3; var x = x + 2; x + x"

-- var x = 3; (var x = x + 2; x + x)
-- var x = 3 + 2; x + x
-- 5 + 5
-- 10

-- "var y = 3; var x = 5 + 2; x + x"

e4 :: Exp
e4 = parseExp "var y = 2 + 3; x + y"

{- Here we added the Declare case:

We need to be careful to detect variable shadowing and avoid 
substituting a variable that is bound locally and happens to have 
the same name as the variable being substituted.

 -}

{- Add one more case for local variable declarations:
  subst (Declare name exp body) = undefined
-}

{- Evaluation using an environment -}

-- evaluate var x = 6; x + x in {} -> 
--
-- program.js
-- | {} |
-- var x = 3;
-- | {x -> 3} |
-- var x = 6; 
-- | {x -> 6, x -> 3} |
-- x + x



  


{- Answer:


substitute1 :: (String,Int) -> Exp -> Exp
substitute1 (var, val) exp = subst exp where
  subst (Number i)      = Number i
  subst (Add a b)       = Add (subst a) (subst b)
  subst (Subtract a b)  = Subtract (subst a) (subst b)
  subst (Multiply a b)  = Multiply (subst a) (subst b)
  subst (Divide a b)    = Divide (subst a) (subst b)
  subst (Variable name) = if var == name
                          then Number val
                          else Variable name
  subst (Declare name exp body) = 
     if (var == name)  
     then Declare name (subst exp) body  -- variable is shadowed, so don't substitute variable in body
     else Declare name (subst exp) (subst body) -- the bound variable is different from "var" so we can substitue in body
-}

{- Here we added the case for Declare (last case) 

evaluation of an expression such as:

var x = 3 + 4; x + 5

proceeds by substituting the bound variable by the corresponding value
(which is the result of evaluating "3+4") in the body of the variable declaration:

substitute x = 7 in x + 5
==>
7 + 5

and then evaluating the resulting expression

7+5
==>
12

IMPORTANT NOTE:

This version of "evaluate" does not define a case for variables:

evaluate (Variable x) = ???

The reason is that we assume that all variables are bound by a local 
variable declaration. This means that expressions such as:

var x = 3; x + 7

or

var x = 3; var y = x + 2; x + y

will work well with evaluation. 

However, evaluating an expression such as:

var y = x + 2; x + y

will result in an evaluation error when the evaluator 
tries to evaluate "x". 

This error corresponds to an undeclared variable error in a language 
like Java or C. In those languages, writing:

int y = x + 2;

(without a prior definition of "x") will result in an error as well.

-}

{-
evaluate :: Exp -> Int
evaluate (Number i)       = i
evaluate (Add a b)        = evaluate a + evaluate b
evaluate (Subtract a b)   = evaluate a - evaluate b
evaluate (Multiply a b)   = evaluate a * evaluate b
evaluate (Divide a b)     = evaluate a `div` evaluate b
evaluate (Declare name exp body) = evaluate (substitute1 (name, evaluate exp) body)
-}
