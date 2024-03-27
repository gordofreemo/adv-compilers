{-# LANGUAGE TupleSections #-}
module CCMachine where

import           AbstractSyntax    as S
import           Data.Maybe
import qualified EvaluationContext as E
import           Latex
import           Utils             as U

type MachineState = (S.Term, E.Context)

ccMachineStep :: MachineState -> Maybe MachineState
ccMachineStep (t, e) = case t of
    S.App t1 t2
        | not (S.isValue t1)                        -> return (t1, E.fillWithContext e (E.AppT E.Hole t2))     {-cc1-}
        | S.isValue t1 && not (S.isValue t2)        -> return (t2, E.fillWithContext e (E.AppV t1 E.Hole))     {-cc2-}
        | S.Abs x _ t12 <- t1                       -> return ((x |-> t2) t12, e)                              {-ccbeta-}
        | otherwise                                 -> error "should there be something here?"
    S.PrimApp p ts -> case span S.isValue ts of                                                                {-ccdelta-}
        (evaluated, rest)
            | null rest                             -> return (S.primOpEval p ts, e)
            | otherwise                             -> return (head rest, E.fillWithContext e (E.PrimApp p evaluated E.Hole (tail rest)))
    S.Closure t1 k                                  -> undefined
    S.If t1 t2 t3
        | S.Const S.Tru <- t1                       -> return (t2, e)
        | S.Const S.Fls <- t1                       -> return (t3, e)
        | not (S.isValue t1)                        -> return (t1, E.fillWithContext e (E.If E.Hole t2 t3))
    S.Fix t1
        | S.Abs x11 _ t11 <- t1                     -> return ((x11 |-> S.Fix t1) t11, e)
        | otherwise                                 -> return (t1, e `E.fillWithContext` E.Fix E.Hole)
    S.Let x t1 t2
        | not (S.isValue t1)                        -> return (t1, e `E.fillWithContext` E.Let1 x E.Hole t2)
        | S.isValue t1 && not (S.isValue t2)        -> return ((x |-> t1) t2, e)
    S.Record lts
        | (vs, (l, t1):ts) <- span (S.isValue . snd) lts  -> return (t1, E.Record vs (l, E.Hole) ts)
    S.Project t1 l1
        | not (S.isValue t1)                        -> return (t1, e `E.fillWithContext` E.Project E.Hole l1)
        | S.Record labelsAndTerms <- t1             -> (, e) <$> lookup l1 labelsAndTerms
        -- | otherwise                                 -> return (t1, e `E.fillWithContext` E.Project E.Hole l1)
    S.Tag l1 t1 tau1                              -> return (t1, e `E.fillWithContext` E.Tag l1 E.Hole tau1)
    S.Case t1 lxts
        | not (S.isValue t1) -> return (t1 , e `E.fillWithContext` E.Case1 E.Hole lxts)
        | S.Tag l' t' _ <- t1 -> let
            (ls, xs, ts) = unzip3 lxts
            x' = fromJust $ lookup l' (zip ls xs)
            tBody = fromJust $ lookup x' (zip xs ts)
            in return ((x' |-> t') tBody, e)

    S.ErrorTerm err                                 -> Nothing
    v | S.isValue v                                 -> shift v e
    x                                               -> error $ show x ++ " is not defined in ccMachineStep"
    where
        shift :: S.Term -> E.Context -> Maybe MachineState
        shift v' E.Hole = Nothing
        shift v' e'     = return (e' `E.fillWithTerm` v', E.Hole)

ccMachineLoop :: MachineState -> Maybe MachineState
ccMachineLoop tc =
  case ccMachineStep tc `U.debug` (show tc) of
    Just tc' -> ccMachineLoop tc' --`debug` show tc
    Nothing  -> Just tc

ccMachineEval :: S.Term -> S.Term
ccMachineEval t =
    case ccMachineLoop (t, E.Hole) of
        Just (v, E.Hole)
            | S.isValue v -> v
            | otherwise   -> error "ccMachineEval: not value at end"
        Just (_, _) -> error "ccMachineEval: not hole at end"
        Nothing -> error "ccMachineEval: not possible"

ccMachineTrace :: MachineState -> [MachineState]
ccMachineTrace tc =
    case ccMachineStep tc of
    Just tc' -> tc:ccMachineTrace tc'
    Nothing  -> []

ccMachineTraceWithLast :: MachineState -> [MachineState]
ccMachineTraceWithLast tc =
    case ccMachineStep tc of
    Just tc' -> tc:ccMachineTraceWithLast tc'
    Nothing  -> [tc]

ccMachineLatexDisplayState :: MachineState -> String
ccMachineLatexDisplayState (t, e) = "$\\langle$" ++ latexShow t ++ ", \\textcolor{blue}{" ++ latexShow e ++ "}$\\rangle$"

ccMachineLatexDisplayEvaluation :: S.Term -> String
ccMachineLatexDisplayEvaluation t =
  foldr (\te s -> ccMachineLatexDisplayState te ++ "\\\\\n" ++ s)
         ""
         trace
  where
    trace = ccMachineTraceWithLast (t, E.Hole)
