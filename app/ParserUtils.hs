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
strSpace :: String -> Parser String
strSpace s = string s <* many1 (oneOf " \n\t")

arrow :: Parser String
arrow = string "->"

lpar :: Parser Char
lpar = char '('

comma :: Parser Char
comma = char ','

rpar :: Parser Char
rpar = char ')'

boolKeyword :: Parser S.Type
boolKeyword = string "Bool" >> return S.TypeBool

intKeyword :: Parser S.Type
intKeyword = string "Int"  >> return S.TypeInt

identifier :: Parser S.Var
identifier = many1 letter

absKeyword :: Parser String
absKeyword = string "abs"

colon :: Parser Char
colon = char ':'

fullstop :: Parser Char
fullstop = char '.'

appKeyword :: Parser String
appKeyword = string "app"

trueKeyword :: Parser S.Term
trueKeyword = string "true" >> return (S.Const S.Tru)

falseKeyword :: Parser S.Term
falseKeyword = string "false" >> return (S.Const S.Fls)

ifKeyword :: Parser String
ifKeyword = string "if"

thenKeyword :: Parser String
thenKeyword = string "then"

elseKeyword :: Parser String
elseKeyword = string "else"

fiKeyword :: Parser String
fiKeyword = string "fi"

intliteral :: Parser S.Term
intliteral = S.Const . S.IntConst <$> fmap read (many1 digit)

plus :: Parser S.PrimOp
plus = char '+' >> return S.IntAdd
-- plus = S.IntAdd <$ char '+' -- these are equivalent

minus :: Parser S.PrimOp
minus = char '-' >> return S.IntSub

mul :: Parser S.PrimOp
mul = char '*' >> return S.IntMul

div :: Parser S.PrimOp
div = char '/' >> return S.IntDiv

nand :: Parser S.PrimOp
nand = char '^' >> return S.IntNand

equal :: Parser S.PrimOp
equal = char '=' >> return S.IntEq

lt :: Parser S.PrimOp
lt = char '<'  >> return S.IntLt

endOfWord :: Parser Char
endOfWord = lpar
    <|> rpar
    <|> colon
    <|> fullstop
    <|> space
