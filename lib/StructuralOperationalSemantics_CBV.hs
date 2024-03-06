{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Use camelCase" #-}
module StructuralOperationalSemantics_CBV where

import           AbstractSyntax as S

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
        _                 -> Left ("Error evaluating " ++ (show t) ++ " with env " ++ (show e))
eval_big t@(S.PrimApp op xs) e = do
    xs_val <- mapM ((flip $ eval_big) e) xs
    return (S.primOpEval op xs_val)
eval_big t e = Left ("Error evaluating term " ++ (show t) ++ " with environment " ++ (show e))



-- Small-step semantics
eval_small :: S.Term -> Either String S.Term
eval_small t -- return self
    | S.Const _  <- t = return t
    | S.Var _    <- t = return t
    | S.Abs {}   <- t = return t
-- error
eval_small (S.ErrorTerm err) = error ("something went wrong! term errored during small step evaluation: " ++ err)
-- pg 103: E-AppAbs
eval_small (S.App (S.Abs x _ t12) v2) | S.isValue v2 = return $ (x |-> v2) t12
-- pg 103: E-App2
eval_small (S.App v1 t2) | S.isValue v1 = do t2' <- eval_small t2; return $ S.App v1 t2'
-- pg 103: E-App1
eval_small (S.App t1 t2) = do t1' <-  eval_small t1; return $ S.App t1' t2
-- pg 144: E-FixBeta
eval_small (S.Fix f@(S.Abs x _ t2)) = return $ (x |-> S.Fix f) t2
-- pg 144: E-Fix
eval_small (S.Fix t1) = do t1' <- eval_small t1; return $ S.Fix t1'
-- pg 124: E-LetV
eval_small (S.Let x v1 t2) | S.isValue v1 = return $ (x |-> v1) t2
-- pg 124: E-Let
eval_small (S.Let x t1 t2) = do t1' <- eval_small t1; return $ S.Let x t1' t2
-- pg 34: E-IfTrue
eval_small (S.If (S.Const S.Tru) t2 _) = return t2
-- pg 34: E-IfFalse
eval_small (S.If (S.Const S.Fls) _ t3) = return t3
-- pg 34: E-If
eval_small (S.If t1 t2 t3) = do t1' <- eval_small t1; return $ S.If t1' t2 t3
-- primops
eval_small (S.PrimApp op xs)
    | all S.isValue xs = return (S.primOpEval op xs)
    | otherwise          = do xs' <- mapM eval_small xs; return $ S.PrimApp op xs'
-- pg 129: E-ProjRcd
eval_small (S.Project t1@(S.Record labelsAndTerms) l1)
    | isValue t1 = lookupOrElse l1 labelsAndTerms (l1 ++ " is not in " ++ show t1)
-- pg 129:E-Prog
eval_small (S.Project t1 l1) = do t1' <- eval_small t1; return $ S.Project t1' l1
-- pg 129: E-Rcd
eval_small (S.Record labelsAndTerms) = do
    let vs = takeWhile (isValue . snd) labelsAndTerms
    let ((l1, t1):ts) = dropWhile (isValue . snd) labelsAndTerms
    t1' <- eval_small t1
    return $ Record (vs ++ [(l1, t1')] ++ ts)
-- pg 136: E-Case-Variant
eval_small (S.Case tag@(S.Tag l1 t1 _) lvt) = do
    let (labels, vars, terms) = unzip3 lvt
    x2 <- S.lookupOrElse l1 (zip labels vars) ("Invalid label in: " ++ show tag)
    t2 <- S.lookupOrElse l1 (zip labels terms) ("Invalid label in: " ++ show tag)
    Right (S.subst x2 t1 t2)
-- pg 136; E-Case
eval_small (S.Case t1 lvt) = do t1' <- eval_small t1; return $ S.Case t1' lvt
-- pg 136 E-Variant
eval_small (S.Tag l1 t1 tau1) = do t1' <- eval_small t1; return $ S.Tag l1 t1' tau1
-- To catch things that are not pattern matched
eval_small t = error (show t ++ " is not defined in eval_small")

eval :: S.Term -> S.Term
eval t
    | S.isValue t = t
    | otherwise  = case eval_small t of
        Right t'            -> eval t'
        Left err            -> S.ErrorTerm err

eval_prime :: S.Term -> S.Term
eval_prime t = case eval_big t S.Empty of
    Right v  -> v
    Left err -> S.ErrorTerm err
