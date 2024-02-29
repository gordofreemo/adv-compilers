module Main where

import           Parser
import           Test.HUnit

testAnswers :: [(FilePath, String)]
testAnswers = [
    ("corelambda_files/test_if", "true")
    -- ("filename", "expected")
    ]

testSingle :: (FilePath, String) -> IO Test
testSingle (file, answer) = do
    res <- runFile file
    putStrLn ("\n Testing " ++ file)
    return (res ~?= answer)

tests :: IO Test
tests = do
    TestList <$> mapM testSingle testAnswers

main :: IO Counts
main = runTestTT =<< tests

