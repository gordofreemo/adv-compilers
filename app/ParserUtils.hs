module ParserUtils where

import qualified AbstractSyntax         as S
import           Text.Parsec.Char
import           Text.Parsec.Combinator
import           Text.Parsec.Prim
import           Text.Parsec.String     (Parser)

-- | parses whitespace, fails if there is no whitespace.
whitespace :: Parser String
whitespace = many $ oneOf " \n\t"



-- | Parses a string that is followed by at least
-- | 1 whitespace character
stringSpace :: String -> Parser String
stringSpace s = string s <* whitespace

charSpace :: Char -> Parser Char
charSpace c = char c <* whitespace

arrow :: Parser String
arrow = stringSpace "->"

lpar :: Parser Char
lpar = charSpace '('

comma :: Parser Char
comma = charSpace ','

rpar :: Parser Char
rpar = charSpace ')'

boolKeyword :: Parser S.Type
boolKeyword = stringSpace "Bool" >> return S.TypeBool

intKeyword :: Parser S.Type
intKeyword = stringSpace "Int"  >> return S.TypeInt

identifier :: Parser S.Var
identifier = many1 letter <* whitespace

absKeyword :: Parser String
absKeyword = stringSpace "abs"

colon :: Parser Char
colon = charSpace ':'

fullstop :: Parser Char
fullstop = charSpace '.'

appKeyword :: Parser String
appKeyword = stringSpace "app"

trueKeyword :: Parser S.Term
trueKeyword = stringSpace "true" >> return (S.Const S.Tru)

falseKeyword :: Parser S.Term
falseKeyword = stringSpace "false" >> return (S.Const S.Fls)

-- | add to avoid the current whitespace issues.
-- keyword :: String -> Parser String
-- keyword s = s <

ifKeyword :: Parser String
ifKeyword = stringSpace "if"

thenKeyword :: Parser String
thenKeyword = stringSpace "then"

elseKeyword :: Parser String
elseKeyword = stringSpace "else"

fiKeyword :: Parser String
fiKeyword = stringSpace "fi"

intliteral :: Parser S.Term
intliteral = S.Const . S.IntConst <$> fmap read (many1 digit) <* whitespace

-- Prim ops, I know there is a better way to do this:
-- primOp :: Parser S.PrimOp
-- primOp = plus <|> minus <|> mul <|> divParser <|> nand <|> equal <|> lt

primOp :: Parser S.PrimOp
primOp = choice [plus, minus, mul, divParser, nand, equal, lt]

-- TODO do we want to just put all of these into primOp function?
plus :: Parser S.PrimOp
plus = charSpace '+' >> return S.IntAdd
-- plus = S.IntAdd <$ charSpace '+' -- these are equivalent

minus :: Parser S.PrimOp
minus = charSpace '-' >> return S.IntSub

mul :: Parser S.PrimOp
mul = charSpace '*' >> return S.IntMul

divParser :: Parser S.PrimOp
divParser = charSpace '/' >> return S.IntDiv

nand :: Parser S.PrimOp
nand = charSpace '^' >> return S.IntNand

equal :: Parser S.PrimOp
equal = charSpace '=' >> return S.IntEq

lt :: Parser S.PrimOp
lt = charSpace '<'  >> return S.IntLt

-- | unused right now
-- endOfWord :: Parser Char
-- endOfWord = lpar
--     <|> rpar
--     <|> colon
--     <|> fullstop
--     <|> space
