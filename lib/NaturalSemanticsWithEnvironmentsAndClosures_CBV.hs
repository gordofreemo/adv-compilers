



module NaturalSemanticsWithEnvironmentsAndClosures_CBV where

import qualified AbstractSyntax    as S
import           Data.List
import           Data.Maybe
import qualified IntegerArithmetic as I

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
  Clo t e   -> t
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
  S.Abs x tau t1  ->  Just (Clo (S.Abs x tau t1) e)
  S.App t1 t2     ->  do
                        Clo (S.Abs x _ t') e' <- evalInEnv e t1
                        v' <- evalInEnv e t2
                        evalInEnv ((x,v'):e') t'
  S.PrimApp op ts -> do
                        vs' <- mapM eval' ts
                        let ts' = valueToTerm <$> vs'
                        let res = S.primOpEval op ts'
                        eval' res
    where eval' t' = e |- t'
  S.If t1 t2 t3   -> do
                        BoolVal b <- evalInEnv e t1
                        if b
                          then evalInEnv e t2
                          else evalInEnv e t3
  S.Let x t1 t2   -> do
                        v' <- evalInEnv e t1
                        evalInEnv ((x,v'):e) t2
  S.Fix t1        -> do
                        Clo (S.Abs x _ t11) e' <- evalInEnv e t1 -- fix (\x.t) -> [x |-> fix (\x.t)] t
                        evalInEnv e' $ (x S.|-> t) t11
  _               -> error ("not valid for nat semantics: " ++ show t)

eval :: S.Term -> Value
eval t = case evalInEnv [] t of
  Just v  -> v
  Nothing -> error "failed to perform Nat Sem"

evalToTerm :: S.Term -> S.Term
evalToTerm = valueToTerm . eval
