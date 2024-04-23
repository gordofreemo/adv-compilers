



module NaturalSemanticsWithEnvironmentsAndClosures_CBV where

import qualified AbstractSyntax    as S
import           Data.List
import           Data.Maybe
import qualified IntegerArithmetic as I
import           Utils             as U

data Value = Clo S.Var S.Term Env
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
  Clo x t1 e   -> foldr subst' t1 e
    where
      subst' :: (S.Var, Value) -> S.Term -> S.Term
      subst' (y, v1) t2 = if y == x then (y S.|-> valueToTerm v1) t2 else t2
  BoolVal b -> if b then S.Const S.Tru else S.Const S.Fls
  IntVal i  -> S.Const $ S.IntConst i
  CharVal c -> S.Const $ S.CharConst c
  UnitVal   -> S.Const S.Unit

(|-) :: Env -> S.Term -> Either String Value
(|-) = evalInEnv

evalInEnv :: Env -> S.Term -> Either String Value
evalInEnv e t = case t `U.debug` (show $ (t, e)) 
  of
  S.Const constant -> case constant of
    S.IntConst i  -> return $ IntVal i
    S.Tru         -> return $ BoolVal True
    S.Fls         -> return $ BoolVal False
    S.CharConst c -> return $ CharVal c
    S.Unit        -> return UnitVal
  S.Var x         ->  U.lookupOrElse x e ("variable '" ++ show t ++ "' not in environment: " ++ show e)
  S.Abs x _ t1  ->  return (Clo x t1 e)
  S.App t1 t2     ->  case evalInEnv e t1 --`U.debug` (show $ evalInEnv e t1)
                          of
                          Right (Clo x t' e') -> do
                            v2' <- evalInEnv e t2
                            evalInEnv ((x,v2'):e') t'
                          Left err -> Left err
                          _ -> Left $ "first term in app was not an abstraction: " ++ show t
  S.PrimApp op ts -> do
                        vs' <- mapM (evalInEnv e) ts
                        opEvalInEnv e op vs'
  S.If t1 t2 t3   -> case evalInEnv e t1 of 
                          Right (BoolVal b) -> if b
                              then evalInEnv e t2
                              else evalInEnv e t3
                          _ -> Left $ "not a bool in " ++ show t
  S.Let x t1 t2   -> do
                        v' <- evalInEnv e t1
                        evalInEnv ((x,v'):e) t2
  -- fix (\x.t) = [x |-> fix (\x.t)] t = (\x.t) (fix (\x.t))
  S.Fix t1        -> do -- i dont think this properly uses closures
                        -- fix (\f.e2) -> [f |-> fix (\f.e2)] e2 == letrec f = (fix (\f.e2)) in e2
                        -- (fix (\f.\x.(x=0?0:x+(f(x-1))))) 3
                        -- (\f.\x.(x=0?0:x+(f(x-1)))) (fix (\f.\x.(x=0?0:x+(f(x-1))))) 3
                        -- \x.(x=0?0:x+((fix (\f.\x.(x=0?0:x+(f(x-1))))) (x-1))) 3
                        -- 
                        case evalInEnv e t1 of --`U.debug` (show $ evalInEnv e t1)
                          Right t1'@(Clo x tBody e') -> do
                            vBody <- evalInEnv e' tBody
                            evalInEnv ((x,vBody):e') tBody
                          -- Right (e1@(Clo f e2 e')) -> evalInEnv e ((f S.|-> t) e2)
                          Left err -> Left err
                          _ -> Left $ "not a closure in fix: " ++ show t

  _               -> error ("not valid for nat semantics: " ++ show t)

-- fixClosure :: String -> Value -> Env -> Value
-- fixClosure x v e = Clo x (S.Fix $ valueToTerm v) e

opEvalInEnv :: Env -> S.PrimOp -> [Value] -> Either String Value
opEvalInEnv e op vs = evalInEnv e $ S.primOpEval op (valueToTerm <$> vs)

eval :: S.Term -> Value
eval t = case evalInEnv [] t of
  Right v  -> v
  Left err -> error err

evalToTerm :: S.Term -> S.Term
evalToTerm = valueToTerm . eval
