module Main where

import           AbstractSyntax
import           Control.Monad
import           Data.Either
import           Parser
import           StructuralOperationalSemantics_CBV
import           System.Environment
import           System.IO
import           Text.Parsec.Char
import           Text.Parsec.Combinator
import           Text.Parsec.Error
import           Text.Parsec.Prim
import           Text.Parsec.String
import           Typing

main :: IO ()
main = do
        [args] <- getArgs;
        inh <- openFile args ReadMode
        file_data <- hGetContents inh
        return ()

parseFile :: String -> IO ()
parseFile fname = do
              inh <- openFile fname ReadMode
              file_data <- hGetContents inh
              let x = case parse (termParser <* eof) "" (file_data) of
                    Left err            -> error (show err)
                    Right parsedProgram -> parsedProgram
              putStrLn ("GIVEN PROGRAM: " ++ show x)
              putStrLn ("Free Variables: " ++ show (fv x))
              putStrLn ("Typechecker: " ++ show (typeCheck x))
              case typeCheck x of
                TypeError _ -> putStrLn "Cannot evaluate program with a Type Error"
                _           -> putStrLn ("Evaluator: " ++ show (eval x))


testParse :: String -> IO ()
testParse s = putStrLn $ case parse (termParser <* eof) "" s of
    Left err -> "!!! ERROR !!! \n" ++ show err
    Right x  -> show x

-- -- | This is just a test and is not used!
-- parentheses :: Parser (String, String)
-- parentheses = (,) <$> (char '(' *> many letter) <*> (char ',' *> many letter <* char ')')
-- -- parentheses = do
-- --     _ <- char '('
-- --     a <- anyChar
-- --     _ <- char ','
-- --     b <- anyChar
-- --     _ <- char ')'
-- --     return (a, b)

-- -- | Removes all whitespace from a string (i.e. ' ', '\n', '\t')
-- removeAllWhitespace :: String -> String
-- removeAllWhitespace []        = []
-- removeAllWhitespace (' ':xs)  = removeAllWhitespace xs
-- removeAllWhitespace ('\n':xs) = removeAllWhitespace xs
-- removeAllWhitespace ('\t':xs) = removeAllWhitespace xs
-- removeAllWhitespace (x:xs)    = x : removeAllWhitespace xs
