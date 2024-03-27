module Main where

import           CCMachine          as CC
import           Parser
import           System.Environment

main :: IO ()
main = do
        [args] <- getArgs;
        parseFile args
        return ()

compileWithCC :: FilePath -> IO ()
compileWithCC fp = do
        program <- parseProgram fp
        print program
        print $ typeCheckTerm program
        let evalResult = CC.ccMachineEval program
        print evalResult
