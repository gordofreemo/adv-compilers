-- | Typed continuation-passing style transformations (for call-by-value!)
module CPS where

import Data.Maybe
import Data.List
import Control.Monad
import Control.Monad.Trans.State

import qualified AbstractSyntax as S
import qualified Typing as T

tau = S.TypeUnit


toCPS_FischerPlotkin :: S.Type -> S.Term -> S.Term
toCPS_FischerPlotkin answerT t = case t of
    -- [[x]] = \k. k x
    S.Var x -> S.Abs k (S.TypeArrow answerT answerT) (S.App tk (S.Var x))
    -- [[c]] = \k. k c
    S.Const c -> S.Abs k (S.TypeArrow answerT answerT) (S.App tk (S.Const c))
    -- [[\x. t1]] = \k. k (\x. [[t1]])
    S.Abs x _ m -> S.Abs k tau (S.App tk (S.Abs x tau (toCPS_FischerPlotkin tau m)))
    -- [[t1 t2]] = \k. [[t1]] (\v1. [[t2]] (\v2. v1 v2 k))
    S.App t1 t2 -> S.Abs k tau 
        (S.App t1CPS (S.Abs v1 tau 
            (S.App t2CPS (S.Abs v2 tau 
                (S.App (S.App tv1 tv2) tk)))))
        where 
            t1CPS = toCPS_FischerPlotkin tau t1 -- [[t1]]
            t2CPS = toCPS_FischerPlotkin tau t2 -- [[t2]]
    -- [[~ t1 t2 .. tn]] = \k. [[t1]] (\x1. [[t2]] (\x2. ... [[tn]] (\xn. k (~ x1 x2 .. xn))))
    --                         {    1    }  {    2    }  ... {    n    }  {     final     }
    --                         We can write a function `f` that does one step and foldl'
    --                   = Abs k (foldl' f final listOfDummyVariables) 
    S.PrimApp op ts -> S.Abs k tau $ foldl' makeOpCPS final (reverse $ zip xNames tsCPS)
        where
            xNames = [replicate i '#' | i <- [1..length ts]]
            xs = S.Var <$> xNames
            tsCPS = toCPS_FischerPlotkin tau <$> ts
            final = S.App tk (S.PrimApp op xs)
            makeOpCPS :: S.Term -> (S.Var, S.Term) -> S.Term
            makeOpCPS t0 (x, tApp) = S.App tApp (S.Abs x tau t0)
    -- [[let x=t1 in t2]] = \k. [[t1]] (\v1. [[t2]] (\v2. let x=v1 in v2 k))
    --                    = \k1. (\k2. k2 (\x. [[t2]])) (\v1. [[t1]] (\v2. v1 v2 k1)) -- this is cheating
    -- S.Let x t1 t2 -> S.Abs k tau 
    --     (S.App t1CPS (S.Abs v1 tau 
    --         (S.App t2CPS (S.Abs v2 tau 
    --             (S.App (S.Let x tv1 tv2) tk))))) -- why does this not work?
    S.Let x t1 t2 -> S.Abs k tau 
        (S.App (S.Abs k tau (S.App tk (S.Abs x tau t2CPS))) 
        (S.Abs v1 tau (S.App t1CPS (S.Abs v2 tau (S.App (S.App tv1 tv2) tk))))) -- cheating
    -- S.Let x t1 t2 -> S.Abs k tau 
    --     (S.App t1CPS (S.Abs v1 tau 
    --         (S.App (S.Let x tv1 t2CPS) tk)))
        where
            t1CPS = toCPS_FischerPlotkin tau t1 -- [[t1]]
            t2CPS = toCPS_FischerPlotkin tau t2 -- [[t2]]
            -- closure1 = S.Abs v1 tau (S.App t2CPS closure2) -- \v1. [[t2]] (\v2. let x=v1 in v2 k)
            -- closure2 = S.Abs v2 tau (S.Let x tv1 (S.App tv2 tk)) -- \v2. let x=v1 in v2 k
    -- [[fix (\f. \x. t2)]] = \k. k (fix \f.\x.[[t2]]) -- ... for fix, only this special shape is handled
    S.Fix (S.Abs f tauf (S.Abs x taux t2)) -> S.Abs k tau (S.App (S.Fix (S.Abs f tauf (S.Abs x taux t2CPS))) tk)
    -- S.Fix (S.Abs f tauf (S.Abs x taux t2)) -> S.Abs k tau 
    --     (S.App t2CPS (S.Abs v2 tau 
    --         ((S.Fix (S.Abs f tauf (S.Abs x taux (S.App tk tv2)))))))
        where
            t2CPS = toCPS_FischerPlotkin tau t2 
    -- [[if t1 then t2 else t3]] = \k. [[t1]] (\v1. if v1 then [[t2]] k else [[t3]] k)
    S.If t1 t2 t3 -> S.Abs k tau 
        (S.App t1CPS (S.Abs v1 tau 
            (S.App t2CPS (S.Abs v2 tau 
                (S.App t3CPS (S.Abs v3 tau
                    (S.App tk (S.If tv1 tv2 tv3))))))))
        where
            t1CPS = toCPS_FischerPlotkin tau t1
            t2CPS = toCPS_FischerPlotkin tau t2
            t3CPS = toCPS_FischerPlotkin tau t3
    tUnknown -> error $ "not supported in CPS: " ++ show tUnknown
    where
        k = "k"
        tk = S.Var "k"
        v1 = "v1"
        tv1 = S.Var "v1"
        v2 = "v2"
        tv2 = S.Var "v2"
        v3 = "v3"
        tv3 = S.Var "v3"
    -- ...

toCPS_DanvyFilinski_HigherOrder :: S.Type -> S.Term -> (S.Term -> S.Term) -> S.Term
toCPS_DanvyFilinski_HigherOrder answerT t = case t of 
    _ -> undefined
    -- ...

{-

-}