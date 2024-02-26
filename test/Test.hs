module Main where

import           Parser
import           Test.HUnit

testAnswers :: [(FilePath, String)]
testAnswers = [
    ("corelambda_files/test_if", "false"),
    ("corelambda_files/test_if", "false")
    ]

testSingle :: (FilePath, String) -> IO Test
testSingle (file, answer) = do
    res <- parseFile file
    return $ TestCase (res @=? answer)

tests :: IO Test
tests = TestList <$> mapM testSingle testAnswers

main :: IO Counts
main = runTestTT =<< tests

