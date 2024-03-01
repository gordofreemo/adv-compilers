module StructuralOperationalSemantics_CBV where

import qualified AbstractSyntax   as S
import           Data.List
import           System.IO.Unsafe

-- Big-step semantics
eval_big :: S.Term -> S.Environment -> Either String S.Term
eval_big t@(S.Const x) _ = Right t
eval_big t@(S.Record _) _ = Right t
eval_big (S.Var x) e = S.lookupEnv x e
eval_big t@(S.Abs _ _ _) e = Right (S.Closure t e) 
eval_big t@(S.App t1 t2) e = case (eval_big t1 e) of
  Right (S.Closure (S.Abs x _ t12) e_prime) -> do
      v2 <- eval_big t2 e
      v <- eval_big t12 (S.Bind (x,v2) e_prime)
      return v
  err@(Left _) -> err
  _            -> Left ("Error evaluating " ++ (show t) ++ " with environment " ++ (show e))
eval_big t@(S.If t1 t2 t3) e = do
      v1 <- eval_big t1 e
      case v1 of
        (S.Const S.Tru) -> do v2 <- eval_big t2 e; return v2
        (S.Const S.Fls) -> do v3 <- eval_big t3 e; return v3
        _               -> Left ("Error evaluating " ++ (show t) ++ " with env " ++ (show e))
eval_big t@(S.PrimApp op xs) e = do
      xs_val <- mapM ((flip $ eval_big) e) xs
      return (S.primOpEval op xs_val)
eval_big t e = Left ("Error evaluating term " ++ (show t) ++ " with environment " ++ (show e))



-- Small-step semantics
eval_small :: S.Term -> Either String S.Term
eval_small t@(S.Const x) = Right t
eval_small t@(S.Record _) = Right t
eval_small t@(S.Var _) = error $ "eval_small should never be called on a Var: " ++ show t
eval_small t@(S.Abs _ _ _) = error $ "eval_small should never be called on an Abs: " ++ show t
-- eval_small t@(S.Const _) = error $ "eval_small should never be called on a Const: " ++ show t
eval_small (S.ErrorTerm _) = error "there should never be an ErrorTerm, remove this"
eval_small (S.App t1@(S.Abs x _ t12) t2)
  |  S.isValue t2 = Right (S.subst x t2 t12)
eval_small (S.App t1 t2)
  |  S.isValue t1 = do t2' <- eval_small t2; Right (S.App t1 t2')
  |  otherwise = do t1' <-  eval_small t1; Right (S.App t1' t2)
eval_small (S.Fix f@(S.Abs x _ t2)) = Right (S.subst x (S.Fix f) t2)  -- pg 144: E-FixBeta
eval_small (S.Fix t1) = do t1' <- eval_small t1; Right (S.Fix t1')  -- pg 144: E-Fix
-- eval_small (S.Let x t1 t2) = do t1' <- eval_small t1; Right (S.subst x t1' t2)
eval_small (S.Let x t1 t2)
  | S.isValue t1 = Right (S.subst x t1 t2)  -- pg 124: E-LetV
  | otherwise = do t1' <- eval_small t1; Right (S.Let x t1' t2)  -- pg 124: E-Let
eval_small (S.If (S.Const S.Tru) t2 t3) = Right t2
eval_small (S.If (S.Const S.Fls) t2 t3)  = Right t3
eval_small (S.If t1 t2 t3) = do t1' <- eval_small t1; Right (S.If t1' t2 t3)
eval_small (S.PrimApp op xs)
  | (all S.isValue xs) = Right (S.primOpEval op xs)
  | otherwise          = do xs' <- mapM eval_small xs; Right (S.PrimApp op xs')
eval_small (S.Project t1 label) = case eval_small t1 of
    Right (S.Record labelsAndTerms) -> S.maybeToEither (lookup label labelsAndTerms) ""
    Left err -> Left err
    _ -> Left (show t1 ++ " is not a Record")
eval_small (S.Case tag@(S.Tag l1 t1 _) lvt) = lookupVarTerm l1 -- pg 136: E-CASE-VARIANT
  where
    (labels, vars, terms) = unzip3 lvt
    lookupVarTerm l = do
      x2 <- S.lookupOrElse l (zip labels vars) ("Invalid label in: " ++ show tag)
      t2 <- S.lookupOrElse l (zip labels terms) ("Invalid label in: " ++ show tag)
      Right (S.subst x2 t1 t2)
eval_small (S.Case t1 lvt) = do t1' <- eval_small t1; Right (S.Case t1' lvt) -- pg 136; E-CASE
eval_small (S.Tag l1 t1 tau1) = do t1' <- eval_small t1; Right (S.Tag l1 t1' tau1) -- pg 136 E-VARIANT
-- eval_small t = error (show t ++ " is not defined in eval_small")

eval :: S.Term -> S.Term
eval t
  | S.isValue t = t
  | otherwise  = case eval_small t of
    Right v@(S.Const x) -> v
    Right t'            -> eval t'
    Left err            -> S.ErrorTerm err

eval_prime :: S.Term -> S.Term
eval_prime t = case eval_big t S.Empty of
  Right v -> v
  Left err -> S.ErrorTerm err