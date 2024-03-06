{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Use tuple-section" #-}
{-# OPTIONS_GHC -Wno-unused-do-bind #-}
module Typing where

import           AbstractSyntax as S hiding (Bind, Empty)
import           Control.Monad
import           Data.List

data Context = Empty | Bind Context S.Var S.Type deriving (Eq)

instance Show Context where
    show Empty = "<>"
    show (Bind gamma x tau) = show gamma ++ "," ++ show x ++ "," ++ ":" ++ show tau

contextLookup :: S.Var -> Context -> Either String S.Type
contextLookup x Empty = Left ("\"" ++ x ++ "\" is a free variable and cannot be type checked.")
contextLookup x (Bind gamma y tau)
        | x == y = Right tau
        | otherwise = contextLookup x gamma

typing :: Context -> S.Term -> Either String S.Type
typing gamma t = case t of
    S.ErrorTerm s -> return $ S.TypeError s
    S.Var x -> contextLookup x gamma
    S.Abs x tau1 t2 -> do
        tau2 <- typing (Bind gamma x tau1) t2
        Right (S.TypeArrow tau1 tau2)
    S.App tFunc tArg -> do
        tauFunc <- typing gamma tFunc
        case tauFunc of
            S.TypeArrow tauFrom tauTo -> enforceType tArg tauFrom gamma >> return tauTo
            _ -> Left (show tFunc ++ " is not an abstraction type")
    S.Let x t1 t2 -> do
        tau1 <- typing gamma t1
        tau2 <- typing (Bind gamma x tau1) t2
        Right tau2
    S.Const S.Tru -> Right S.TypeBool
    S.Const S.Fls -> Right S.TypeBool
    S.If t1 t2 t3 -> do
        enforceType t1 S.TypeBool gamma
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
    S.PrimApp op args -> do
        let (tauArgs, tau) = S.primOpType op
        zipWithM_ (\t' tau' -> enforceType t' tau' gamma) args tauArgs
        return tau
    -- S.PrimApp S.IntAdd [t1,t2] -> arith t1 t2
    -- S.PrimApp S.IntSub [t1,t2] -> arith t1 t2
    -- S.PrimApp S.IntMul [t1,t2] -> arith t1 t2
    -- S.PrimApp S.IntDiv [t1,t2] -> arith t1 t2
    -- S.PrimApp S.IntNand [t1,t2] -> arith t1 t2
    -- S.PrimApp S.IntEq [t1,t2] -> rel t1 t2
    -- S.PrimApp S.IntLt [t1,t2] -> rel t1 t2
    -- S.PrimApp S.CharOrd [t1] -> enforceType t1 S.TypeChar gamma >> Right S.TypeInt
    -- S.PrimApp S.CharChr [t1] -> enforceType t1 S.TypeInt gamma >> Right S.TypeChar
    -- S.PrimApp op _ -> Left ("Invalid arguments applied to " ++ show op ++ "in: " ++ show t)
    S.Fix (S.Abs x tau1 t2) -> do
        let gamma' = Bind gamma x tau1
        enforceType t2 tau1 gamma'
    S.Fix badT1 -> Left $ "Fix takes a function, not a " ++ show (typing gamma badT1) ++ " in " ++ show badT1
    S.Record labelsAndTerms -> do
        types <- mapM (typing gamma) terms
        return $ S.TypeRecord $ zip labels types
        where
            (labels, terms) = unzip labelsAndTerms
    S.Project tOuter labelInner -> case typing gamma tOuter of
        Right (S.TypeRecord labelsAndTypes) -> do
            S.maybeToEither (lookup labelInner labelsAndTypes) ("the Label " ++ show labelInner ++ " is not a valid label in: " ++ show t)
        _ -> Left ("'" ++ show tOuter ++ "' is not a Record in project statement: \"" ++ show t ++ "\"")
    S.Tag tagLabel t1 tau -> case tau of
        S.TypeVariant labelsAndTypes -> do
            tau1 <- S.lookupOrElse tagLabel labelsAndTypes ("the Label " ++ show tagLabel ++ " is not a valid " ++ show tau ++ " in " ++ show t)
            enforceType t1 tau1 gamma
            return tau
        _ -> Left ("'" ++ show tau ++ "' is not a Variant in tag statement: \"" ++ show t ++ "\"")
    S.Case t1 lvt -> case typing gamma t1 of -- needs more checks
        Right tau1@(S.TypeVariant labelsAndTypes) -> do
            if sort labelsBody == sort labelsFocus
                then isSameType
                else Left (show tau1 ++ " do not have the same labels as " ++ show t)
            where
                (labelsBody, vars, terms) = unzip3 lvt
                (labelsFocus, _) = unzip labelsAndTypes
                helper :: S.Label -> S.Var -> S.Term -> Either String S.Type
                helper lA varA tB = do
                    tauA <- S.lookupOrElse lA labelsAndTypes ("label \'" ++ lA ++ "\' is not valid for " ++ show tau1 ++ " in: " ++ show t)
                    let gamma' = Bind gamma varA tauA
                    typing gamma' tB
                typesBody = sequence $ zipWith3 helper labelsBody vars terms
                allSameType :: [S.Type] -> Either String S.Type
                allSameType [] = Left "Must have at least one element in "
                allSameType [x] = Right x
                allSameType (x:y:ys)
                    | x == y = allSameType (y:ys)
                    | otherwise = Left ("all paths must return the same type in the case statement: " ++ show t)
                isSameType = allSameType =<< typesBody
        Left x -> Left x
        _ -> Left ("'" ++ show t1 ++ "' is not a Variant in case statement: \"" ++ show t ++ "\"")
    tUnknown -> error ("typing for " ++ show tUnknown ++ " is not implemented")
    where
        enforceType tGiven tauExpected gamma' = do
            tauGiven <- typing gamma' tGiven
            if tauExpected == tauGiven
                then Right tauExpected
                else Left ("Expected a '" ++ show tauExpected ++ ",' but given the '"
                        ++ show tauGiven ++ "' " ++ show tGiven ++ " in: \"" ++ show t ++ "\"")
        arith t1 t2 = do
            enforceType t1 S.TypeInt gamma
            enforceType t2 S.TypeInt gamma
            Right S.TypeInt
        rel t1 t2 = do
            enforceType t1 S.TypeInt gamma
            enforceType t2 S.TypeInt gamma
            Right S.TypeBool

typeCheck :: S.Term -> S.Type
typeCheck t =
    case typing Empty t of
        Right tau   -> tau
        Left errMsg -> S.TypeError errMsg

