module Parser where

import qualified AbstractSyntax         as S
import           ParserUtils
import           Prelude                hiding (div)
import           Text.Parsec.Combinator
import           Text.Parsec.Prim
import           Text.Parsec.String     (Parser)

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
typeParser = try (S.TypeArrow <$> (arrow *> lpar *> typeParser) <*> (comma *> typeParser <* rpar))
         <|> try boolKeyword
         <|> try intKeyword

-- | Why would we not have this operator?
-- (<!>) :: t1 -> ParsecT s u m a -> t2
-- p1 <!> p2 = p1 <!> try p2

termParser :: Parser S.Term
termParser =
        try (S.Abs <$> (absKeyword *> lpar *> identifier)
                   <*> (colon *> typeParser)
                   <*> (fullstop *> termParser) <* rpar)
    <|> try (S.App <$> (appKeyword *> lpar *> termParser)
                   <*> (comma *> termParser) <* rpar)
    <|> try trueKeyword
    <|> try falseKeyword
    <|> try (S.If <$> (ifKeyword *> termParser)
                  <*> (thenKeyword *> termParser)
                  <*> (elseKeyword *> termParser) <* fiKeyword)
    <|> try intliteral
    <|> try (S.PrimApp <$> primOp
                       <*> (lpar *> termParser `sepBy1` comma) <* rpar)
    <|> try (lpar *> termParser <* rpar)
    <|> (S.Var <$> identifier)

    -- These are all the same
    -- <|> plus lpar Term comma Term rpar
    -- <|> minus lpar Term comma Term rpar
    -- <|> mul lpar Term comma Term rpar
    -- <|> div lpar Term comma Term rpar
    -- <|> nand lpar Term comma Term rpar
    -- <|> equal lpar Term comma Term rpar
    -- <|> lt lpar Term comma Term rpar

