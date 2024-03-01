module Main where

import           Parser
import           Test.HUnit

data TestType = NoParseError
              | NoTypeError
              | SolutionIs String
              | FreeVars [String]
              | EvalsToSomething

testAnswers :: [(FilePath, TestType)]
testAnswers = [
    -- ("filename", "expected")
    ("corelambda_files/cbntest1.corelambda", NoTypeError), -- diverges
    ("corelambda_files/intFact.corelambda", SolutionIs "120"),
    ("corelambda_files/sumn100.corelambda", SolutionIs "5050"),
    ("corelambda_files/takFastOpts.corelambda", NoTypeError), -- did not evaluate
    ("corelambda_files/takTest1Opts.corelambda", NoTypeError), -- did not evaluate
    ("corelambda_files/takTestOpts.corelambda", SolutionIs "6"),
    ("corelambda_files/test11.corelambda", SolutionIs "false"),
    ("corelambda_files/test12.corelambda", SolutionIs "8"),
    ("corelambda_files/test13.corelambda", SolutionIs "720"),
    ("corelambda_files/test14.corelambda", SolutionIs "false"),
    ("corelambda_files/test15.corelambda", SolutionIs "'b'"),
    ("corelambda_files/test16.corelambda", EvalsToSomething),
    ("corelambda_files/test17.corelambda", EvalsToSomething),
    ("corelambda_files/test18.corelambda", EvalsToSomething),
    ("corelambda_files/test21.corelambda", EvalsToSomething),
    ("corelambda_files/test23.corelambda", EvalsToSomething),
    ("corelambda_files/test24.corelambda", EvalsToSomething),
    ("corelambda_files/test25.corelambda", EvalsToSomething),
    ("corelambda_files/test26.corelambda", EvalsToSomething),
    ("corelambda_files/test27.corelambda", EvalsToSomething),
    ("corelambda_files/test28.corelambda", EvalsToSomething),
    ("corelambda_files/test28prime.corelambda", EvalsToSomething),
    ("corelambda_files/test39.corelambda", EvalsToSomething),
    ("corelambda_files/test40.corelambda", EvalsToSomething),
    ("corelambda_files/test_fix", EvalsToSomething),
    ("corelambda_files/test_freevar", FreeVars ["z"]),
    ("corelambda_files/test_if", SolutionIs "true"),
    ("corelambda_files/test_primop", NoTypeError), -- diverges
    ("corelambda_files/test_types", EvalsToSomething)
    ]

testSingle :: (FilePath, TestType) -> IO Test
testSingle (file, NoParseError) = do
    parsedFile <- parseOnly file
    return $ TestCase $ assertBool ("ParseError in " ++ file) (parsedFile /= Nothing)
testSingle (file, NoTypeError) = do
    typeOfFile <- parseThenTypeCheck file
    return $ TestCase $ assertBool ("TypeError in " ++ file) (typeOfFile /= Nothing)
testSingle (file, SolutionIs expected) = do
    actual <- runFile file
    return $ TestCase $ assertBool ("Actual ["++show actual++"] /= Expected ["++show expected++"] in " ++ file) (expected == actual)
testSingle (file, (FreeVars varsExpected)) = do
    maybeVarsActual <- parseThenCheckFreeVar file
    case maybeVarsActual of
        Just varsActual -> return $ TestCase $ assertBool ("Incorrect FreeVars in " ++ file) (varsExpected == varsActual)
        Nothing -> assertFailure ("ParseError in " ++ file)
testSingle (file, EvalsToSomething) = do
    putStrLn ("Evaluating " ++ file)
    res <- runFile file
    return $ TestCase $ assertBool res (take 3 res /= "!!!")


tests :: IO Test
tests = do
    TestList <$> mapM testSingle testAnswers

main :: IO Counts
main = runTestTT =<< tests

