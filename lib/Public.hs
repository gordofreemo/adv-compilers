{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE InstanceSigs     #-}
module Public where

import qualified AbstractSyntax                     as S
import qualified CCMachine                          as CC
import           Control.Applicative
import           Control.Exception                  (SomeException, catch)
import           Control.Monad
import           MyErrorType
import qualified Parser                             as P
import qualified ReductionSemantics                 as RS
import qualified StructuralOperationalSemantics_CBV as CBV
import           System.IO
import           System.IO.Unsafe
import           Text.Parsec
import qualified Typing                             as T

-- import qualified

type TermEvaluator = S.Term -> S.Term
type Term = S.Term

parser :: FilePath -> Result S.Term
parser fname = do
    case parse P.programParser "" fileData of
        Right success -> Valid success
        Left err      -> ParseError $ show err
    where
        fileData = unsafePerformIO $ hGetContents =<< openFile fname ReadMode

typeChecker :: S.Term -> Result S.Term
typeChecker t = case T.typeCheck t of
    S.TypeError err -> TypeError err
    _               -> Valid t

evaluateWith :: (String, TermEvaluator) -> S.Term -> Result S.Term
evaluateWith (evalName, evaluator) t = case evaluator t of
    S.ErrorTerm err -> EvaluationError evalName err
    t'              -> Valid t'

evalWithReductionSemantics :: TermEvaluator
evalWithReductionSemantics = RS.textualMachineEval

evalWithCCMachine :: TermEvaluator
evalWithCCMachine = CC.ccMachineEval

evalWithCBV :: TermEvaluator
evalWithCBV = CBV.eval

evalWithDeBruijn :: TermEvaluator
evalWithDeBruijn = undefinedEvaluator "DeBruijn evaluator"

evalWithNatSemantics :: TermEvaluator
evalWithNatSemantics = undefinedEvaluator "Natural Semantics evaluator"

undefinedEvaluator :: String -> TermEvaluator
undefinedEvaluator name t = S.ErrorTerm $ name ++ " is undefined"
