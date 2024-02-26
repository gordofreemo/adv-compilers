module ParserUtils where

import qualified AbstractSyntax         as S
import           Control.Monad
import           Text.Parsec.Char
import           Text.Parsec.Combinator
import           Text.Parsec.Error
import           Text.Parsec.Prim
import           Text.Parsec.String

comment :: Parser String
comment = try (string "--" *> manyTill anyChar (choice [newline, fmap (const '\\') eof ]))
      <|> string "{-" *> manyTill anyChar (try $ string "-}")

-- | parses whitespace, fails if there is no whitespace.
whitespace :: Parser [String]
whitespace = many $ choice [many1 (oneOf " \n\t"), comment]

-- | Parses a string that is followed by at least
-- | 1 whitespace character
stringSpace :: String -> Parser String
stringSpace s = string s <* whitespace

charSpace :: Char -> Parser Char
charSpace c = char c <* whitespace

arrow :: Parser String
arrow = stringSpace "->"

thickArrow :: Parser String
thickArrow = stringSpace "=>"

lpar :: Parser Char
lpar = charSpace '('

comma :: Parser Char
comma = charSpace ','

rpar :: Parser Char
rpar = charSpace ')'

bar :: Parser Char
bar = charSpace '|'

identifier :: Parser S.Var
identifier = do
    identifierString <- many1 letter
    when (identifierString `elem` keywordList) (fail "Keywords cannot be identifiers")
    _ <- notFollowedBy letter
    _ <- whitespace
    return identifierString

var :: Parser S.Var
var = identifier

label :: Parser S.Var
label = identifier

colon :: Parser Char
colon = charSpace ':'

fullstop :: Parser Char
fullstop = charSpace '.'

-- | add to avoid the current whitespace issues.
keyword :: String -> Parser String
keyword s = if s `elem` keywordList
    then string s <* notFollowedBy letter <* whitespace
    else error (s ++ " is not a keyword and cannot be used as one unless in keywordList. ")

-- | alias for keyword
kw :: String -> Parser String
kw = keyword

keywordList :: [String]
keywordList = [
    "Bool", "Int",
    "abs", "app", "fix",
    "true", "false",
    "if", "then", "else", "fi",
    "let", "in", "end",
    "Record", "record", "project",
    "Variant", "case", "of", "esac",
    "tag", "as"]

-- variantTypeKeyword :: Parser String
-- variantTypeKeyword = keyword "Variant"

-- recordTypeKeyword :: Parser String
-- recordTypeKeyword = keyword "Record"

-- boolTypeKeyword :: Parser String
-- boolTypeKeyword = keyword "Bool"

-- intTypeKeyword :: Parser String
-- intTypeKeyword = keyword "Int"

-- projectKeyword :: Parser String
-- projectKeyword = keyword "project"

-- caseKeyword :: Parser String
-- caseKeyword = keyword "case"

-- ofKeyword :: Parser String
-- ofKeyword = keyword "of"

-- esacKeyword :: Parser String
-- esacKeyword = keyword "esac"

-- tagKeyword :: Parser String
-- tagKeyword = keyword "tag"

-- asKeyword :: Parser String
-- asKeyword = keyword "as"

-- recordKeyword :: Parser String
-- recordKeyword = keyword "record"

-- letKeyword :: Parser String
-- letKeyword = keyword "let"

-- inKeyword :: Parser String
-- inKeyword = keyword "in"

-- endKeyword :: Parser String
-- endKeyword = keyword "end"

-- fixKeyword :: Parser String
-- fixKeyword = keyword "fix"

-- absKeyword :: Parser String
-- absKeyword = keyword "abs"

-- appKeyword :: Parser String
-- appKeyword = keyword "app"

-- trueKeyword :: Parser S.Term
-- trueKeyword = S.Const S.Tru <$ keyword "true"

-- falseKeyword :: Parser S.Term
-- falseKeyword = S.Const S.Fls <$ keyword "false"

-- ifKeyword :: Parser String
-- ifKeyword = keyword "if"

-- thenKeyword :: Parser String
-- thenKeyword = keyword "then"

-- elseKeyword :: Parser String
-- elseKeyword = keyword "else"

-- fiKeyword :: Parser String
-- fiKeyword = keyword "fi"

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

tuple :: Parser a -> Parser [a]
tuple p = lpar *> (p `sepBy1` comma) <* rpar

-- | unused right now
-- endOfWord :: Parser Char
-- endOfWord = lpar
--     <|> rpar
--     <|> colon
--     <|> fullstop
--     <|> space
