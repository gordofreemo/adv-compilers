module StructuralOperationalSemantics_CBV where

import Data.List
import qualified AbstractSyntax as S

eval1 :: S.Term -> Maybe S.Term
eval1 t = case t of
  S.App (S.Abs x tau11 t12) t2
    |  S.isValue t2 -> Just (S.subst x t2 t12)
  S.App t1 t2
    |  S.isValue t1 -> do t2' <- eval1 t2; Just (S.App t1 t2')
    |  otherwise -> do t1' <-  eval1 t1; Just (S.App t1' t2)
  S.If (S.Const S.Tru) t2 t3 -> Just t2
  S.If (S.Const S.Fls) t2 t3 -> Just t3
  S.If t1 t2 t3 -> do t1' <- eval1 t1; Just (S.If t1' t2 t3)
  S.PrimApp op xs -> do xs' <- (sequence $ fmap eval1 xs); Just (S.primOpEval op xs')
  S.Const x -> Just t
  _ -> Nothing

eval :: S.Term -> S.Term
eval t =
  case eval1 t of
    Just v@(S.Const x) -> v
    Just t' -> eval t'

