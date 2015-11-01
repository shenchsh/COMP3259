module Value where
import Prelude hiding (LT, GT, EQ, id)

data Value = IntV  Int
           | BoolV Bool
 deriving (Eq)

type Env = [(String, Value)]

{- TODO: unary and binary -} 

unary :: UnaryOp -> Value -> Value
unary Neg (IntV i) = IntV (-i)
unary Not (BoolV b) = BoolV (not b)
unary _ _ = error "Type Error!"

binary :: BinaryOp -> Value -> Value -> Value
binary Add (IntV i1) (IntV i2) = IntV (i1+i2)
binary Mul (IntV i1) (IntV i2) = IntV (i1*i2)
binary EQ  (IntV i1) (IntV i2) = BoolV (i1 == i2)
binary EQ  (BoolV i1) (BoolV i2) = BoolV (i1 == i2)
binary _ _ _ = error "Type error!"

data BinaryOp = Add | Sub | Mul | Div | And | Or
              | GT | LT | LE | GE | EQ
  deriving (Show, Eq)

data UnaryOp = Neg | Not  -- -5, not true 
  deriving (Show, Eq)


instance Show Value where
  show (IntV n) = show n
  show (BoolV True) = "true"
  show (BoolV False) = "false"



{- Answers:


unary Not (BoolV b) = BoolV (not b)
unary Neg (IntV i)  = IntV (-i)

binary Add (IntV a)  (IntV b)  = IntV (a + b)
binary Sub (IntV a)  (IntV b)  = IntV (a - b)
binary Mul (IntV a)  (IntV b)  = IntV (a * b)
binary Div (IntV a)  (IntV b)  = IntV (a `div` b)
binary And (BoolV a) (BoolV b) = BoolV (a && b)
binary Or  (BoolV a) (BoolV b) = BoolV (a || b)
binary LT  (IntV a)  (IntV b)  = BoolV (a < b)
binary LE  (IntV a)  (IntV b)  = BoolV (a <= b)
binary GE  (IntV a)  (IntV b)  = BoolV (a >= b)
binary GT  (IntV a)  (IntV b)  = BoolV (a > b)
binary EQ  a         b         = BoolV (a == b)
-}