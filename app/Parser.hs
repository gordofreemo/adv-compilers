module Parser where

import qualified AbstractSyntax     as S
import           ParserUtils
import           Prelude            hiding (div)
import           Text.Parsec.Prim
import           Text.Parsec.String (Parser)

{-
Type -->
arrow lpar Type comma Type rpar
| BoolKeyword
| IntKeyword

Term -->
identifier
| absKeyword lpar identifier colon Type fullstop Term rpar
| appKeyword lpar Term comma Term rpar
| trueKeyword
| falseKeyword
| ifKeyword Term thenKeyword Term elseKeyword Term fiKeyword
| intliteral
| plus lpar Term comma Term rpar
| minus lpar Term comma Term rpar
| mul lpar Term comma Term rpar
| div lpar Term comma Term rpar
| nand lpar Term comma Term rpar
| equal lpar Term comma Term rpar
| lt lpar Term comma Term rpar
| lpar Term rpar
-}

typeParser :: Parser S.Type
typeParser = S.TypeArrow <$> (arrow *> lpar *> typeParser) <*> (comma *> typeParser <* rpar)
    <|> boolKeyword
    <|> intKeyword

termParser :: Parser S.Term
termParser =
        try (S.Abs <$> (absKeyword *> lpar *> identifier)
                   <*> (colon *> typeParser)
                   <*> (fullstop *> termParser) <* rpar)
    <|> try (S.App <$> (appKeyword *> lpar *> termParser)
                   <*> (comma *> termParser) <* rpar)
    <|> trueKeyword
    <|> falseKeyword
    <|> S.If <$> (ifKeyword *> termParser <* thenKeyword)
             <*> (termParser <* elseKeyword)
             <*> (termParser <* fiKeyword)
    <|> intliteral
    -- These are all the same
    -- <|> plus lpar Term comma Term rpar
    -- <|> minus lpar Term comma Term rpar
    -- <|> mul lpar Term comma Term rpar
    -- <|> div lpar Term comma Term rpar
    -- <|> nand lpar Term comma Term rpar
    -- <|> equal lpar Term comma Term rpar
    -- <|> lt lpar Term comma Term rpar
    <|> lpar *> termParser <* rpar

