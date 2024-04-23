-- | Typed continuation-passing style transformations (for call-by-value!)
module CPS where

import Data.Maybe
import Data.List
import Control.Monad
import Control.Monad.Trans.State

import qualified AbstractSyntax as S
import qualified Typing as T

toCPS_FischerPlotkin :: S.Type -> S.Term -> S.Term
toCPS_FischerPlotkin answerT t = case t of
    S.Var x -> S.Abs k kType (S.App tk (S.Var x))
    S.Const c -> S.Abs k kType (S.App tk (S.Const c))
    S.Abs x tau t1 -> S.Abs k kType (S.App tk (S.Abs x tau (toCPS_FischerPlotkin answerT t1)))
    S.App t1 t2 -> S.Abs k kType (S.App (toCPS_FischerPlotkin S.TypeUnit t1) 
                                        (S.Abs v1 S.TypeUnit (S.App (toCPS_FischerPlotkin answerT t2)
                                                                    (S.Abs v2 S.TypeUnit (S.App tv1 (S.App tv2 tk))))))
    S.PrimApp op ts -> undefined
    S.Let x t1 t2 -> undefined
    S.Fix (S.Abs f tauf (S.Abs x taux t2)) -> undefined -- ... for fix, only this special shape is handled
    S.If {} -> undefined
    _ -> error $ "not supported in CPS"
    where
        k = "k"
        tk = S.Var "k"
        kType = S.TypeArrow answerT answerT
        v1 = "v1"
        tv1 = S.Var "v1"
        v2 = "v2"
        tv2 = S.Var "v2"
    -- ...

toCPS_DanvyFilinski_HigherOrder :: S.Type -> S.Term -> (S.Term -> S.Term) -> S.Term
toCPS_DanvyFilinski_HigherOrder answerT t = case t of 
    _ -> undefined
    -- ...

