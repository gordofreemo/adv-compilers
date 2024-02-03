module Main where

import           Data.Either
import           Parser
import           Text.Parsec.Char
import           Text.Parsec.Combinator
import           Text.Parsec.Error
import           Text.Parsec.Prim
import           Text.Parsec.String


main :: IO ()
main = print "Hello, World"

testParse :: String -> IO ()
testParse s = putStrLn $ case parse (termParser <* eof) "" s of
    Left err -> "!!! ERROR !!! \n" ++ show err
    Right x  -> show x

parentheses :: Parser (String, String)
parentheses = (,) <$> (char '(' *> many letter) <*> (char ',' *> many letter <* char ')')
-- parentheses = do
--     _ <- char '('
--     a <- anyChar
--     _ <- char ','
--     b <- anyChar
--     _ <- char ')'
--     return (a, b)
