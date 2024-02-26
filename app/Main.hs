module Main where

import           Parser
import           System.Environment

main :: IO ()
main = do
        [args] <- getArgs;
        parseFile args
        return ()

