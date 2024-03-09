module EvaluationContext where

import qualified AbstractSyntax as S
import           Latex

data Context  =  Hole
              |  AppT        Context S.Term
              |  AppV        S.Term Context -- where Term is a value
              |  If          Context S.Term S.Term
              |  Fix         Context
              |  PrimApp     S.PrimOp [S.Term] Context [S.Term] -- where Terms before the Context are values
              |  Let1        S.Var Context S.Term
              |  Let2        S.Var S.Term Context
              |  Record      [(S.Label, S.Term)] (S.Label, Context) [(S.Label, S.Term)]
              |  Project     Context S.Label
              |  Tag         S.Label Context S.Type
              |  Case1       Context [(S.Label, S.Var, S.Term)]
              |  Case2       S.Term [(S.Label, S.Var, S.Term)] (S.Label, S.Var, Context) [(S.Label, S.Var, S.Term)]
              deriving (Eq, Show)

instance LatexShow Context where
    latexShow c = "unimplemented: latexShow for EvaluationContext.Context"

fillWithTerm :: Context -> S.Term -> S.Term
fillWithTerm c t = case c of
    Hole                                ->  t
    AppT c1 t2                          ->  S.App (fillWithTerm c1 t) t2
    AppV t1 c2                          ->  S.App t1 (fillWithTerm c2 t)
    If c1 t2 t3                         ->  S.If (fillWithTerm c1 t) t2 t3
    Fix c1                              ->  S.Fix (fillWithTerm c1 t)
    PrimApp p ts1 c1 ts2                ->  S.PrimApp p (ts1 ++ [fillWithTerm c1 t] ++ ts2)
    Let1 x c1 t2                        ->  S.Let x (fillWithTerm c1 t) t2
    Let2 x t1 c2                        ->  S.Let x t1 (fillWithTerm c2 t)
    Record qPre (l1, c1) qPost          ->  S.Record (qPre ++ [(l1, fillWithTerm c1 t)] ++ qPost)
    Project c1 l1                       ->  S.Project (c1 `fillWithTerm` t) l1
    Tag l1 c1 tau1                      ->  S.Tag l1 (c1 `fillWithTerm` t) tau1
    Case1 c1 q                          ->  S.Case (c1 `fillWithTerm` t) q
    Case2 t1 qPre (l2, x2, c2) qPost    ->  S.Case t1 (qPre ++ [(l2, x2, fillWithTerm c2 t)] ++ qPost) 

fillWithContext :: Context -> Context -> Context
fillWithContext c c' = case c of
    Hole                                ->  c'
    AppT c1 t2                          ->  AppT (fillWithContext c1 c') t2
    AppV t1 c2                          ->  AppV t1 (fillWithContext c2 c')
    If c1 t2 t3                         ->  If (fillWithContext c1 c') t2 t3
    Fix c1                              ->  Fix (fillWithContext c1 c')
    PrimApp p ts1 c1 ts2                ->  PrimApp p ts1 (fillWithContext c1 c') ts2
    Let1 x c1 t2                        ->  Let1 x (fillWithContext c1 c) t2
    Let2 x t1 c2                        ->  Let2 x t1 (fillWithContext c2 c)
    Record qPre (l1, c1) qPost          ->  Record qPre (l1, fillWithContext c1 c) qPost
    Project c1 l1                       ->  Project (c1 `fillWithContext` c) l1
    Tag l1 c1 tau1                      ->  Tag l1 (c1 `fillWithContext` c) tau1
    Case1 c1 q                          ->  Case1 (c1 `fillWithContext` c) q
    Case2 t1 qPre (l2, x2, c2) qPost    ->  Case2 t1 qPre (l2, x2, fillWithContext c2 c) qPost 

