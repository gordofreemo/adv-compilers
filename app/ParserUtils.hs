module ParserUtils where

import qualified AbstractSyntax         as S
import           Control.Monad
import           Text.Parsec.Char
import           Text.Parsec.Combinator
import           Text.Parsec.Prim
import           Text.Parsec.String

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

identifier :: Parser S.Var
identifier = do
    identifierString <- many1 letter
    when (identifierString `elem` keywordList) (fail "Keywords cannot be identifiers")
    notFollowedBy letter
    whitespace
    return identifierString

keywordList :: [String]
keywordList = [
    "Bool", "Int", "abs", "app", "true",
    "false", "if", "then", "else", "fi"]

colon :: Parser Char
colon = charSpace ':'

fullstop :: Parser Char
fullstop = charSpace '.'

-- | add to avoid the current whitespace issues.
keyword :: String -> Parser String
keyword s = string s <* notFollowedBy letter <* whitespace

boolKeyword :: Parser S.Type
boolKeyword = keyword "Bool" >> return S.TypeBool

intKeyword :: Parser S.Type
intKeyword = keyword "Int"  >> return S.TypeInt

absKeyword :: Parser String
absKeyword = keyword "abs"

appKeyword :: Parser String
appKeyword = keyword "app"

trueKeyword :: Parser S.Term
trueKeyword = S.Const S.Tru <$ keyword "true"

falseKeyword :: Parser S.Term
falseKeyword = S.Const S.Fls <$ keyword "false"

ifKeyword :: Parser String
ifKeyword = keyword "if"

thenKeyword :: Parser String
thenKeyword = keyword "then"

elseKeyword :: Parser String
elseKeyword = keyword "else"

fiKeyword :: Parser String
fiKeyword = keyword "fi"

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
