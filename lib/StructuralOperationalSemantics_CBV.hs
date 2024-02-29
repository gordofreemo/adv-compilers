module StructuralOperationalSemantics_CBV where

import qualified AbstractSyntax   as S
import           Data.List
import           System.IO.Unsafe

{-
  S.App (S.Fix (S.Abs x xTau t12)) t2 -- does not work
    |  S.isValue t2 -> do
          f <- eval1 $ S.subst x (S.Fix (S.Abs x xTau t12)) t12
          Right (S.subst x t2 f)
-}

eval1 :: S.Term -> Either String S.Term
eval1 (S.App t1@(S.Abs x _ t12) t2)
  |  S.isValue t2 = Right (S.subst x t2 t12)
eval1 (S.App t1 t2)
  |  S.isValue t1 = do t2' <- eval1 t2; Right (S.App t1 t2')
  |  otherwise = do t1' <-  eval1 t1; Right (S.App t1' t2)
eval1 (S.Fix t@(S.Abs x tau t2)) = Right (S.subst x (S.Fix t) t2)
eval1 (S.Fix t1) = do t1' <- eval1 t1; Right (S.Fix t1')
eval1 (S.Let x t1 t2) = do t1' <- eval1 t1; Right (S.subst x t1' t2)
-- eval1 (S.Let x t1 t2)
--   | S.isValue t1 = Right (S.subst x t1 t2)
--   | otherwise = do t1' <- eval1 t1; Right (S.Let x t1' t2)
eval1 (S.If (S.Const S.Tru) t2 t3) = Right t2
eval1 (S.If (S.Const S.Fls) t2 t3)  = Right t3
eval1 (S.If t1 t2 t3) = do t1' <- eval1 t1; Right (S.If t1' t2 t3)
eval1 (S.PrimApp op xs)
  | (all S.isValue xs) = Right (S.primOpEval op xs)
  | otherwise          = do xs' <- mapM eval1 xs; Right (S.PrimApp op xs')
eval1 t@(S.Const x) = Right t
eval1 (S.Project t1 label) = case eval1 t1 of
    Right (S.Record labelsAndTerms) -> S.maybeToEither (lookup label labelsAndTerms) ""
    Left err -> Left err
    _ -> Left (show t1 ++ " is not a Record")
eval1 t@(S.Record _) = Right t
eval1 (S.Case tag@(S.Tag l1 t1 tau1) lvt) = lookupVarTerm l1
  where
    (labels, vars, terms) = unzip3 lvt
    lookupVarTerm l = do
      x2 <- S.lookupOrElse l (zip labels vars) ("Invalid label in: " ++ show tag)
      t2 <- S.lookupOrElse l (zip labels terms) ("Invalid label in: " ++ show tag)
      Right (S.subst x2 t1 t2)
eval1 (S.Case t1 lvt) = do t1' <- eval1 t1; Right (S.Case t1' lvt)
-- eval1 tag@(S.Tag l1 t1 tau1) = Right tag -- How is this supposed ot be handled?
eval1 t = error (show t ++ " is not defined in eval1")

eval :: S.Term -> S.Term
eval t =
  case eval1 t of
    Right v@(S.Const x) -> v
    Right t'            -> eval t'
    Left err            -> error err

