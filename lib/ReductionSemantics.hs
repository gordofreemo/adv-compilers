













module ReductionSemantics where

import Latex
import Data.Maybe
import qualified AbstractSyntax as S
import qualified EvaluationContext as E

makeEvalContext :: S.Term -> Maybe (S.Term, E.Context)
makeEvalContext t = case t of
  S.App (S.Abs x tau11 t12) t2
    |  S.isValue t2 -> Just (t, E.Hole)
  S.App t1 t2
    |  S.isValue t1 -> do (t2', c2) <- makeEvalContext t2; Just (t2', E.AppV t1 c2)
    |  otherwise -> do (t1', c1) <- makeEvalContext t1; Just (t1', E.AppT c1 t2)
  ...
  _ -> Nothing

makeContractum :: S.Term -> S.Term
makeContractum t = case t of
  S.App (S.Abs x tau11 t12) t2               ->  S.subst x t2 t12
  ...
  _                                          ->  error "makeContractum: not a redex"

textualMachineStep :: S.Term -> Maybe S.Term
textualMachineStep t = do (t1, c) <- makeEvalContext t; Just (E.fillWithTerm c (makeContractum t1))

textualMachineEval :: S.Term -> S.Term
textualMachineEval t =
  case textualMachineStep t of
    Just t' -> textualMachineEval t'
    Nothing -> t

textualMachineTrace :: S.Term -> [S.Term]
textualMachineTrace t =
  case textualMachineStep t of
    Just t' -> t:textualMachineTrace t'
    Nothing -> []


