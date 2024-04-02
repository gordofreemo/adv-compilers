module StructuralOperationalSemantics_CBV_forDeBruijn where

import           Data.List
import           Data.Maybe
import           DeBruijnWithIntegerLabelsAndTags as S
import           Utils                            as U

eval1 :: S.Term -> Maybe S.Term
eval1 t = case t of
    -- cant evaluate a value any further
    v | S.isValue v -> return v
    -- pg 103: E-AppAbs
    S.App (S.Abs _ t12) v2
        | S.isValue v2 -> return (S.shift 0 (-1) (S.subst 0 (S.shift 0 1 v2) t12))
    S.App t1 t2
        -- pg 103: E-App2
        | S.isValue t1 -> do t2' <- eval1 t2; Just (S.App t1 t2')
        -- pg 103: E-App1
        | otherwise -> do t1' <-   eval1 t1; Just (S.App t1' t2)
    -- PrimApp
    S.PrimApp op ts -> return $ S.primOpEval op ts
    -- pg 144: E-FixBeta
    S.Fix f@(S.Abs _ t2) -> return $ (0 |-> S.Fix f) t2
    -- pg 144: E-Fix
    S.Fix t1 -> do t1' <- eval1 t1; return $ S.Fix t1'
    -- pg 124: E-LetV
    S.Let v1 t2 | S.isValue v1 -> return (S.shift 0 (-1) (S.subst 0 (S.shift 0 1 v1) t2))
    -- pg 124: E-Let
    S.Let t1 t2 -> do t1' <- eval1 t1; return $ S.Let t1' t2
    -- pg 34: E-IfTrue
    S.If (S.Const S.Tru) t2 _ -> return t2
    -- pg 34: E-IfFalse
    S.If (S.Const S.Fls) _ t3 -> return t3
    -- pg 34: E-If
    S.If t1 t2 t3 -> do t1' <- eval1 t1; return $ S.If t1' t2 t3
    -- primops
    S.PrimApp op xs
        | all S.isValue xs -> return (S.primOpEval op xs)
        | otherwise          -> do xs' <- mapM eval1 xs; return $ S.PrimApp op xs'
    -- pg 129: E-ProjRcd
    S.Project t1@(S.Record ts) i1 zero
        | isValue t1 -> return $ ts!!i1
    -- pg 129:E-Prog
    S.Project t1 i1 i2 -> do t1' <- eval1 t1; return $ S.Project t1' i1 i2
    -- pg 129: E-Rcd
    S.Record ts -> do
        let vs = takeWhile S.isValue ts
        let (tN:rest) = dropWhile S.isValue ts
        tN' <- eval1 tN
        return $ Record (vs ++ [tN'] ++ rest)
    -- pg 136: E-Case-Variant
    -- S.Case t1@(S.Tag i11 t11 _) _ its -> do
    --     let (index, terms) = unzip its
    --     tBody <- lookup i11 its
    --     return $ (0 |-> t11) tBody
    -- pg 136; E-Case
    S.Case t1 tau1 its -> do t1' <- eval1 t1; return $ S.Case t1' tau1 its
    -- pg 136 E-Variant
    S.Tag i1 t1 tau1 -> do t1' <- eval1 t1; return $ S.Tag i1 t1' tau1
    _ -> error $ "unsupported DB term: " ++ show t


eval :: S.Term -> S.Term
eval t =
    case eval1 t of
      Just t' -> eval t'
      Nothing -> t


