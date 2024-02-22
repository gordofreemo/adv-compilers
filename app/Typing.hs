module Typing where

import qualified AbstractSyntax as S
import           Data.List
import           Data.Maybe

data Context = Empty | Bind Context S.Var S.Type deriving (Eq)

instance Show Context where
    show Empty = "<>"
    show (Bind gamma x tau) = show gamma ++ "," ++ show x ++ "," ++ ":" ++ show tau

-- enforceType :: S.Term -> S.Type -> String -> Either String S.Type
-- enforceType t tau errMsg = case t of
--                                 typing

contextLookup :: S.Var -> Context -> Either String S.Type
contextLookup x Empty = Left ("\"" ++ x ++ "\" is a free variable and cannot be type checked.")
contextLookup x (Bind gamma y tau)
            | x == y = Right tau
            | otherwise = contextLookup x gamma

typing :: Context -> S.Term -> Either String S.Type
typing gamma t = case t of
        S.Var x -> contextLookup x gamma
        S.Abs x tau1 t2 -> do
                tau2 <- typing (Bind gamma x tau1) t2
                Right (S.TypeArrow tau1 tau2)
        S.App tFunc tArg -> do
                tauFunc <- typing gamma tFunc
                -- tauArg <- typing gamma tArg
                case tauFunc of
                        S.TypeArrow tauFrom tauTo -> enforceType tArg tauFrom >> return tauTo
                        _ -> Left (show tFunc ++ " is not an abstraction type")
        S.Let x t1 t2 -> do
                tau1 <- typing gamma t1
                tau2 <- typing (Bind gamma x tau1) t2
                Right tau2
        S.Const S.Tru -> Right S.TypeBool
        S.Const S.Fls -> Right S.TypeBool
        S.If t1 t2 t3 -> do
                enforceType t1 S.TypeBool
                tau2 <- typing gamma t2
                tau3 <- typing gamma t3
                if tau2 == tau3
                        then Right tau2
                        else Left ("The '" ++ show tau2 ++ "' \""
                                ++ show t2 ++ "\" and '" ++ show tau3 ++ "' \""
                                ++ show t3 ++ "\" do not have the same type in \"" ++ show t ++ "\".")
        S.Const (S.IntConst _) -> Right S.TypeInt
        S.Const (S.CharConst _) -> Right S.TypeChar
        S.Const S.Unit -> Right S.TypeUnit
        S.PrimApp S.IntAdd [t1,t2] -> arith t1 t2
        S.PrimApp S.IntSub [t1,t2] -> arith t1 t2
        S.PrimApp S.IntMul [t1,t2] -> arith t1 t2
        S.PrimApp S.IntDiv [t1,t2] -> arith t1 t2
        S.PrimApp S.IntNand [t1,t2] -> arith t1 t2
        S.PrimApp S.IntEq [t1,t2] -> rel t1 t2
        S.PrimApp S.IntLt [t1,t2] -> rel t1 t2
        _ -> Left "Invalid Program"
        where
                enforceType tGiven tauExpected = do
                        tauGiven <- typing gamma tGiven
                        if tauExpected == tauGiven
                                then Right tauExpected
                                else Left ("Expected a '" ++ show tauExpected ++ ",' but given the '"
                                                ++ show tauGiven ++ "' " ++ show tGiven ++ " in: \"" ++ show t ++ "\"")
                arith t1 t2 = do
                        enforceType t1 S.TypeInt
                        enforceType t2 S.TypeInt
                        Right S.TypeInt
                rel t1 t2 = do
                        enforceType t1 S.TypeInt
                        enforceType t2 S.TypeInt
                        Right S.TypeBool

typeCheck :: S.Term -> S.Type
typeCheck t =
        case typing Empty t of
            Right tau   -> tau
            Left errMsg -> S.TypeError errMsg

