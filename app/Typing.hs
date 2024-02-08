module Typing where

import qualified AbstractSyntax as S
import           Data.List
import           Data.Maybe

data Context = Empty | Bind Context S.Var S.Type deriving (Eq)

instance Show Context where
    show Empty = "<>"
    show (Bind gamma x tau) = show gamma ++ "," ++ show x ++ "," ++ ":" ++ show tau

contextLookup :: S.Var -> Context -> Maybe S.Type
contextLookup x Empty = Nothing
contextLookup x (Bind gamma y tau)
            | x == y = Just tau
            | otherwise = contextLookup x gamma

typing :: Context -> S.Term -> Maybe S.Type
typing gamma t = case t of
        S.Var x -> contextLookup x gamma
        S.Abs x tau1 t2 -> do tau2 <- typing (Bind gamma x tau1) t2; Just (S.TypeArrow tau1 tau2)
        S.App t1 t2 -> do
                S.TypeArrow tau11 tau12 <- typing gamma t1
                tau <- typing gamma t2
                if tau == tau11 then Just tau12 else Nothing
        S.Const S.Tru -> Just S.TypeBool
        S.Const S.Fls -> Just S.TypeBool
        S.If t1 t2 t3 -> do
                S.TypeBool <- typing gamma t1
                tau <- typing gamma t2
                tau' <- typing gamma t3
                if tau' == tau then Just tau else Nothing
        S.Const (S.IntConst _) -> Just S.TypeInt
        S.PrimApp S.IntAdd [t1,t2] -> arith t1 t2
        S.PrimApp S.IntSub [t1,t2] -> arith t1 t2
        S.PrimApp S.IntMul [t1,t2] -> arith t1 t2
        S.PrimApp S.IntDiv [t1,t2] -> arith t1 t2
        S.PrimApp S.IntNand [t1,t2] -> arith t1 t2
        S.PrimApp S.IntEq [t1,t2] -> rel t1 t2
        S.PrimApp S.IntLt [t1,t2] -> rel t1 t2
        _ -> Nothing
        where
            arith t1 t2 = do S.TypeInt <- typing gamma t1; S.TypeInt <- typing gamma t2; Just S.TypeInt
            rel t1 t2 = do S.TypeInt <- typing gamma t1; S.TypeInt <- typing gamma t2; Just S.TypeBool

typeCheck :: S.Term -> S.Type
typeCheck t =
        case typing Empty t of
            Just tau -> tau
            _        -> error "type error"

