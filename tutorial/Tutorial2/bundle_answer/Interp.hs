module Interp where

import Parser
import Declare
import Prelude hiding (Either(..))


-- | Evaluation function
--
-- Examples:
--
-- >>> evaluate (Neg (Num 3))
-- -3
--
-- >>> evaluate (Power (Num 2) (Num 3))
-- 8
evaluate :: Exp -> Int
evaluate (Num n) = n
evaluate (Add a b) = evaluate a + evaluate b
evaluate (Sub a b) = evaluate a - evaluate b
evaluate (Mult a b) = evaluate a * evaluate b
evaluate (Div a b) = evaluate a `div` evaluate b
evaluate (Neg a) = negate (evaluate a)
evaluate (Power a b) = evaluate a ^ evaluate b


-- | Simple calculator
--
-- Examples:
--
-- >>> calc "1 + 8 * 2"
-- 17
--
-- >>> calc "2 * (8 + -6) ^ 3"
-- 16
calc :: String -> Int
calc  = evaluate . parseExpr


-- Error Handling

data Either a b = Left a | Right b deriving Show

safeHead :: [a] -> Either String a
safeHead []     = Left "can't access the head of an empty list"
safeHead (x:_) = Right x


-- | Evaluation function, revisited
--
-- Examples:
--
-- >>> evaluate2 (Add (Sub (Num 3) (Num 2)) (Mult (Num 2) (Num 3)))
-- Right 7
--
-- >>> evaluate2 (Div (Num 2) (Num 0))
-- Left "Divide by zero: 0"
--
-- >>> evaluate2 (Power (Num 2) (Num (-3)))
-- Left "Raised to a negative number: -3"
evaluate2 :: Exp -> Either String Int
evaluate2 (Num n) = Right n
evaluate2 (Add a b) =
  case evaluate2 a of
    Left msg -> Left msg
    Right a' ->
      case evaluate2 b of
        Left msg -> Left msg
        Right b' -> Right (a' + b')
evaluate2 (Sub a b) =
  case evaluate2 a of
    Left msg -> Left msg
    Right a' ->
      case evaluate2 b of
        Left msg -> Left msg
        Right b' -> Right (a' - b')
evaluate2 (Mult a b) =
  case evaluate2 a of
    Left msg -> Left msg
    Right a' ->
      case evaluate2 b of
        Left msg -> Left msg
        Right b' -> Right (a' * b')
evaluate2 (Div a b) =
  case evaluate2 a of
    Left msg -> Left msg
    Right a' ->
      case evaluate2 b of
        Left msg -> Left msg
        Right b' ->
          if b' == 0
            then Left ("Divide by zero: " ++ show b)
            else Right (a' `div` b')
evaluate2 (Neg a) =
  case evaluate2 a of
    Left msg -> Left msg
    Right a' -> Right (negate a')
evaluate2 (Power a b) =
  case evaluate2 a of
    Left msg -> Left msg
    Right a' ->
      case evaluate2 b of
        Left msg -> Left msg
        Right b' ->
          if b' < 0
            then Left ("Raised to a negative number: " ++ show b)
            else Right (a' ^ b')

calc2 :: String -> Either String Int
calc2  = evaluate2 . parseExpr


-- | Monadic binding operation
--
-- Examples:
-- >>> bindE (Right 3) (\n -> Right (n + 1))
-- Right 4
--
-- >>> bindE (Left "bad things") (\n -> Right (n + 1))
-- Left "bad things"
bindE :: Either a b -> (b -> Either a b) -> Either a b
bindE v f =
  case v of
    Left msg -> Left msg
    Right b  -> f b


evaluate3 :: Exp -> Either String Int
evaluate3 (Num n) = Right n
evaluate3 (Add a b) =
  evaluate3 a `bindE`
  (\a' -> evaluate3 b `bindE`
          (\b' -> Right (a' + b')))
evaluate3 (Sub a b) =
  evaluate3 a `bindE`
  (\a' -> evaluate3 b `bindE`
          (\b' -> Right (a' - b')))
evaluate3 (Mult a b) =
  evaluate3 a `bindE`
  (\a' -> evaluate3 b `bindE`
          (\b' -> Right (a' * b')))
evaluate3 (Div a b) =
  evaluate3 a `bindE`
  (\a' -> evaluate3 b `bindE`
          (\b' -> if b' == 0
                    then Left ("Divide by zero: " ++ show b)
                    else Right (a' `div` b')))
evaluate3 (Neg a) =
  evaluate3 a `bindE`
  (Right . negate)
evaluate3 (Power a b) =
  evaluate3 a `bindE`
  (\a' -> evaluate3 b `bindE`
          (\b' -> if b' < 0
                    then Left ("Raised to a negative number: " ++ show b)
                    else Right (a' ^ b')))

-- | Our glorious calculator
--
-- Examples:
--
-- >>> calc3 "1 + 8 * 2"
-- Right 17
--
-- >>> calc3 "2 * (8 + -6) ^ 3"
-- Right 16
--
-- >>> calc3 "2 * (4 - 2) / (8 - 8)"
-- Left "Divide by zero: (8 - 8)"
--
-- >>> calc3 "2 * (4 - 2) ^ (6 - 8)"
-- Left "Raised to a negative number: (6 - 8)"
calc3 :: String -> Either String Int
calc3  = evaluate3 . parseExpr
