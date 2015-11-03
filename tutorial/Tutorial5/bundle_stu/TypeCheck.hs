module TypeCheck where

import Declare
import Prelude hiding (LT, GT, EQ)

type TEnv = [(String,Type)]

type TFunEnv = [(String, (TEnv, Type))]

tunary :: UnaryOp -> Type -> Maybe Type
tunary Neg TInt = Just TInt
tunary Not TBool = Just TBool
tunary _ _ = Nothing

tbinary :: BinaryOp -> Type -> Type -> Maybe Type
tbinary Add TInt  TInt  = Just TInt
tbinary Sub TInt  TInt  = Just TInt
tbinary Mult TInt  TInt  = Just TInt
tbinary Div TInt  TInt  = Just TInt
tbinary And TBool TBool = Just TBool
tbinary Or  TBool TBool = Just TBool
tbinary LT  TInt  TInt  = Just TBool
tbinary LE  TInt  TInt  = Just TBool
tbinary GE  TInt  TInt  = Just TBool
tbinary GT  TInt  TInt  = Just TBool
tbinary EQ  t1    t2    | t1 == t2 = Just TBool
tbinary _ _ _ = Nothing

-- data Program = Program FunEnv Exp
-- 
-- type FunEnv = [(String, Function)]
-- 
-- data Function = Function [(String, Type)] Exp

-- type TEnv = [(String,Type)]

-- type TFunEnv = [(String, (TEnv, Type))]

checkFunEnv :: FunEnv -> Maybe TFunEnv
checkFunEnv fds = checkFunEnv1 fds [] -- starts with an empty function type environment
  where
    checkFunEnv1 :: FunEnv -> TFunEnv -> Maybe TFunEnv
    checkFunEnv1 [] tfenv = Just tfenv
    checkFunEnv1 ((s, Function tenv e):xs) tfenv =
        case returnType of
            Nothing -> Nothing
            Just a -> checkFunEnv1 xs newTfenv
                        where 
                            newTfenv = (s, (tenv, a)):tfenv
        where
            returnType = tcheck e tenv tfenv
    -- checkFunEnv1 = error "TODO: Question 6"


            
tcheck :: Exp -> TEnv -> TFunEnv -> Maybe Type
tcheck (Call name args) tenv fenv = 
    case lookup name fenv of
        Nothing -> Nothing
        Just (tempTEnv, returnType) ->
            case (comp tempTEnv args) of
                False -> Nothing
                True -> Just returnType
            where
                comp :: TEnv -> [Exp] -> Bool
                comp [] [] = True
                comp ((s, t):xs) (y:ys) = if (Just t == tcheck y tenv fenv) then comp xs ys else False


-- error "TODO: Question 7"
tcheck (Lit v) _ _ =
  case v of
    IntV _ -> Just TInt
    BoolV _ -> Just TBool
tcheck (Unary op e) tenv fenv =
  case tcheck e tenv fenv of
    Just t  -> tunary op t
    Nothing -> Nothing
tcheck (Bin op e1 e2) tenv fenv =
  case (tcheck e1 tenv fenv, tcheck e2 tenv fenv) of
    (Just t1, Just t2) -> tbinary op t1 t2
    _                  -> Nothing
tcheck (If e1 e2 e3) tenv fenv =
  case tcheck e1 tenv fenv of
    Just TBool ->
      case tcheck e2 tenv fenv of
        Just t1 ->
          if Just t1 ==
             tcheck e3 tenv fenv
            then Just t1
            else Nothing
        Nothing -> Nothing
    _ -> Nothing
tcheck (Var v) tenv _ = lookup v tenv
tcheck (Decl v e1 e2) tenv fenv =
  case tcheck e1 tenv fenv of
    Just t  -> tcheck e2 ((v, t) : tenv) fenv
    Nothing -> Nothing


checkProgram :: Program -> Bool
checkProgram (Program funEnv main) =
    case checkFunEnv funEnv of
        Just tfe -> case (tcheck main [] tfe) of
                        Just t -> True
                        Nothing -> False
        Nothing -> False

-- error "TODO: Question 8"
