{-# LANGUAGE TupleSections #-}
module ReductionSemantics where

import           AbstractSyntax    as S
import           Data.Maybe
import qualified EvaluationContext as E
import           Latex

makeEvalContext :: S.Term -> Maybe (S.Term, E.Context)
makeEvalContext t = case t of
  S.App (S.Abs x tau11 t12) t2 -- (λx:T11.t11) t2 -> ((λx:T11.t11) t2, □)
    |  S.isValue t2 -> Just (t, E.Hole)
  S.App t1 t2
    -- v1 t2 -> (t2, v1 □)
    |  S.isValue t1 -> do (t2', c2) <- makeEvalContext t2; Just (t2', E.AppV t1 c2)
    -- t1 t2 -> (t1, □ t2)
    |  otherwise -> do (t1', c1) <- makeEvalContext t1; Just (t1', E.AppT c1 t2)
  S.PrimApp p ts -> case span S.isValue ts of
    (evaluated, rest)  {-ccdelta-}
      | null rest                             -> undefined
      | otherwise                             -> undefined
  S.If t1 t2 t3
    -- if True then t2 else t3 -> (t2, □)
    | S.Const S.Tru <- t1                       -> return (t2, E.Hole)
    -- if False then t2 else t3 -> (t3, □)
    | S.Const S.Fls <- t1                       -> return (t3, E.Hole)
    -- if t1 then t2 else t3 -> (t1, if □ then t2 else t3)
    | not (S.isValue t1)                        -> return (t1, E.If E.Hole t2 t3)
  S.Fix t1
    -- fix (λx.t11) -> ([x ↦ fix (λx.t11)] t11, □)
    | S.Abs x _ t11 <- t1                     -> return ((x |-> S.Fix t1) t11, E.Hole)
    -- fix t1 -> (t1, fix □)
    | otherwise                                 -> return (t1, E.Fix E.Hole)
  S.Let x t1 t2
    -- let x = t1 in t2 -> (t1, let x = □ in t2)
    | not (S.isValue t1)                        -> return (t1, E.Let1 x E.Hole t2)
    -- let x = v1 in t2 -> ([x ↦ v1] t2, □)
    | S.isValue t1 && not (S.isValue t2)        -> return ((x |-> t1) t2, E.Hole)
  S.Record labelsAndTerms
    | undefined -> undefined
  S.Project t1 l
    -- project t1.l -> (t1, project □.l)
    | not (S.isValue t1)                        -> return (t1, E.Project E.Hole l)
    -- project (record (... l=v ...)).l -> (v, □)
    | S.Record labelsAndTerms <- t1             -> (, E.Hole) <$> lookup l labelsAndTerms
    -- ?
    | otherwise                                 -> return (t1, E.Project E.Hole l)
  S.Tag l1 t1 tau1 -- tag (l = t1 as Varient(... l:T1 ...)) ->                    
    -> return (t1, E.Tag l1 E.Hole tau1)
  S.Case t1 lvt                                   -> undefined
  S.ErrorTerm err                                 -> Nothing
  -- v | S.isValue v                                 -> shift v e
  _ -> Nothing

makeContractum :: S.Term -> S.Term
makeContractum t = case t of
  S.App (S.Abs x tau11 t12) t2 ->  (x |-> t2) t12
  -- ...
  _                            ->  error "makeContractum: not a redex"

textualMachineStep :: S.Term -> Maybe S.Term
textualMachineStep t = do (t1, c) <- makeEvalContext t; Just (E.fillWithTerm c (makeContractum t1))

textualMachineEval :: S.Term -> S.Term
textualMachineEval t =
  maybe t textualMachineEval (textualMachineStep t)

textualMachineTrace :: S.Term -> [S.Term]
textualMachineTrace t =
  case textualMachineStep t of
    Just t' -> t:textualMachineTrace t'
    Nothing -> []


