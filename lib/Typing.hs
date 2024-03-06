{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Use tuple-section" #-}
{-# OPTIONS_GHC -Wno-unused-do-bind #-}
module Typing where

import           AbstractSyntax as S hiding (Bind, Empty)
import           Control.Monad
import           Data.List
import           ErrorMessages  as ErrMsg

data Context = Empty | Bind Context S.Var S.Type deriving (Eq)

instance Show Context where
    show Empty = "<>"
    show (Bind gamma x tau) = show gamma ++ "," ++ show x ++ "," ++ ":" ++ show tau


contextLookup :: S.Var -> Context -> Either String S.Type
contextLookup x Empty = Left ("\"" ++ x ++ "\" is a free variable and cannot be type checked.")
contextLookup x (Bind gamma y tau)
        | x == y = Right tau
        | otherwise = contextLookup x gamma

-- TODO: add references to TAPL to make sure these are all correct
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
            _ -> Left (show tFunc ++ " is not an arrow type")
    S.Let x t1 t2 -> do
        tau1 <- typing gamma t1
        tau2 <- typing (Bind gamma x tau1) t2
        Right tau2
    S.Const c -> return $ constType c
    S.If t1 t2 t3 -> do
        enforceType t1 S.TypeBool gamma
        tau2 <- typing gamma t2
        tau3 <- typing gamma t3
        if tau2 == tau3
            then Right tau2
            else Left $ ErrMsg.ifMismatch (t2, tau2, t3, tau3, t)
    S.PrimApp op args -> do
        let (tauArgs, tau) = S.primOpType op
        zipWithM_ (\t' tau' -> enforceType t' tau' gamma) args tauArgs
        return tau
    S.Fix t1
        | (S.Abs x tauX tBody) <- t1 -> do
            let gamma' = Bind gamma x tauX
            enforceType tBody tauX gamma'
        | otherwise -> do
            tau1 <- typing gamma t1
            Left $ ErrMsg.fixErr (tau1, t)
    S.Record labelsAndTerms -> do
        let (labels, terms) = unzip labelsAndTerms
        types <- mapM (typing gamma) terms
        return $ S.TypeRecord $ zip labels types
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
        Right tauNotVariant -> Left $ ErrMsg.notVariantInCase (t1, t)
    tUnknown -> error ("typing for " ++ show tUnknown ++ " is not implemented")
    where
        enforceType :: Term -> Type -> Context -> Either String Type
        enforceType tGiven tauExpected gamma' = do
            tauGiven <- typing gamma' tGiven
            if tauExpected == tauGiven
                then Right tauExpected
                else Left $ ErrMsg.wrongType (tGiven, tauGiven, tauExpected, t)


typeCheck :: S.Term -> S.Type
typeCheck t =
    case typing Empty t of
        Right tau   -> tau
        Left errMsg -> S.TypeError errMsg

