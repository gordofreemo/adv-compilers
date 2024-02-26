module StructuralOperationalSemantics_CBV where

import qualified AbstractSyntax   as S
import           Data.List
import           System.IO.Unsafe

eval1 :: S.Term -> Either String S.Term
eval1 t = case t of
  S.App (S.Abs x _ t12) t2
    |  S.isValue t2 -> Right (S.subst x t2 t12)
  S.App (S.Fix (S.Abs x xTau t12)) t2 -- does not work
    |  S.isValue t2 -> do
          f <- eval1 $ S.subst x (S.Fix (S.Abs x xTau t12)) t12
          Right (S.subst x t2 f)
  S.App t1 t2
    |  S.isValue t1 -> do t2' <- eval1 t2; Right (S.App t1 t2')
    |  otherwise -> do t1' <-  eval1 t1; Right (S.App t1' t2)
  S.Let x t1 t2 -> do t1' <- eval1 t1; Right (S.subst x t1' t2)
  S.If (S.Const S.Tru) t2 t3 -> Right t2
  S.If (S.Const S.Fls) t2 t3 -> Right t3
  S.If t1 t2 t3 -> do t1' <- eval1 t1; Right (S.If t1' t2 t3)
  S.PrimApp op xs -> do xs' <- mapM eval1 xs; Right (S.primOpEval op xs')
  S.Const x -> Right t
  x -> Left $ show x

eval :: S.Term -> S.Term
eval t =
  case eval1 t of
    Right v@(S.Const x) -> v
    Right t'            -> eval t'
    Left err            -> error (err ++ " failed in eval!!!")

