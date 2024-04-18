



module NaturalSemanticsWithEnvironmentsAndClosures_CBV where

import qualified AbstractSyntax    as S
import           Data.List
import           Data.Maybe
import qualified IntegerArithmetic as I
import           Utils             as U

data Value = Clo S.Term Env
           | BoolVal Bool
           | IntVal I.IntegerType
           | CharVal Char
           | UnitVal
          --  | RecordVal [(S.Label,Value)]
          --  | TagVal S.Label Value
          --  | FoldVal Value
           deriving Show

type Env = [(S.Var, Value)]

valueToTerm :: Value -> S.Term
valueToTerm v = case v of
  Clo t e   -> foldr subst' t e
    where
      subst' :: (S.Var, Value) -> S.Term -> S.Term
      subst' (x, v1) = x S.|-> valueToTerm v1
  BoolVal b -> if b then S.Const S.Tru else S.Const S.Fls
  IntVal i  -> S.Const $ S.IntConst i
  CharVal c -> S.Const $ S.CharConst c
  UnitVal   -> S.Const S.Unit

(|-) :: Env -> S.Term -> Maybe Value
(|-) = evalInEnv

evalInEnv :: Env -> S.Term -> Maybe Value
evalInEnv e t = case t of
  S.Const constant -> case constant of
    S.IntConst i  -> return $ IntVal i
    S.Tru         -> return $ BoolVal True
    S.Fls         -> return $ BoolVal False
    S.CharConst c -> return $ CharVal c
    S.Unit        -> return UnitVal
  S.Var x         ->  lookup x e
  S.Abs x tau t1  ->  return (Clo (S.Abs x tau t1) e)
  S.App t1 t2     ->  do
                        Clo (S.Abs x _ t') e' <- evalInEnv e t1 -- `U.debug` (show $ evalInEnv e t1)
                        v' <- evalInEnv e t2
                        evalInEnv ((x,v'):e') t'
  S.PrimApp op ts -> do
                        vs' <- mapM eval' ts
                        opEvalInEnv e op vs'
    where eval' = evalInEnv e
  S.If t1 t2 t3   -> do
                        BoolVal b <- evalInEnv e t1
                        if b
                          then evalInEnv e t2
                          else evalInEnv e t3
  S.Let x t1 t2   -> do
                        v' <- evalInEnv e t1
                        evalInEnv ((x,v'):e) t2
  -- fix (\x.t) = [x |-> fix (\x.t)] t = (\x.t) (fix (\x.t))
  S.Fix t1        -> do -- i dont think this properly uses closures
                        Clo (S.Abs x _ t11) e' <- evalInEnv e t1
                        evalInEnv e' $ (x S.|-> t) t11 -- this line might be illegal
                        -- (Clo (S.Abs x _ tBody) e') <- evalInEnv e t1 `U.debug` ("FIX:  " ++ show (evalInEnv e t1))
                        -- t' <- evalInEnv e t
                        -- evalInEnv ((x,t'):e') tBody
  _               -> error ("not valid for nat semantics: " ++ show t)

opEvalInEnv :: Env -> S.PrimOp -> [Value] -> Maybe Value
opEvalInEnv e op vs = evalInEnv e $ S.primOpEval op (valueToTerm <$> vs)

eval :: S.Term -> Value
eval t = case evalInEnv [] t of
  Just v  -> v
  Nothing -> error "failed to perform Nat Sem"

evalToTerm :: S.Term -> S.Term
evalToTerm = valueToTerm . eval
