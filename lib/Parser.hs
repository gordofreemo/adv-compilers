module Parser (parseFile) where

import qualified AbstractSyntax                     as S
import           ParserUtils
import           Prelude                            hiding (div)
import           StructuralOperationalSemantics_CBV as SOS
import           System.IO
import           Text.Parsec.Combinator
import           Text.Parsec.Prim                   hiding (label)
import           Text.Parsec.String                 (Parser)
import           Typing                             as T

programParser :: Parser S.Term
programParser = whitespace *> termParser <* eof

typeAnnotation :: Parser (S.Label, S.Type)
typeAnnotation = (,) <$> label
                     <*> (colon *> typeParser)

recordInternal :: Parser (S.Label, S.Term)
recordInternal = (,) <$> label
                     <*> (equal *> termParser)

caseInternal :: Parser (S.Label, S.Var, S.Term)
caseInternal = (,,) <$> label
                    <*> (equal *> var)
                    <*> (thickArrow *> termParser)

caseOptions :: Parser [(S.Label, S.Var, S.Term)]
caseOptions = caseInternal `sepBy1` bar

typeParser :: Parser S.Type
typeParser = try (S.TypeArrow <$> (arrow *> lpar *> typeParser) <*> (comma *> typeParser <* rpar))
         <|> try (S.TypeBool <$ kw "Bool")
         <|> try (S.TypeInt <$ kw "Int")
         <|> try (S.TypeRecord <$> (kw "Record" *> tuple typeAnnotation))
         <|> try (S.TypeVariant <$> (kw "Variant" *> tuple typeAnnotation))

termParser :: Parser S.Term
termParser =
        try (S.Var <$> var)
    <|> try intliteral
    <|> try (S.Const S.Tru <$ kw "true")
    <|> try (S.Const S.Fls <$ kw "false")
    <|> try (S.Abs <$> (kw "abs" *> lpar *> identifier)
                   <*> (colon *> typeParser)
                   <*> (fullstop *> termParser) <* rpar)
    <|> try (S.Fix <$> (kw "fix" *> lpar *> termParser) <* rpar) -- not implemented
    <|> try (S.App <$> (kw "app" *> lpar *> termParser)
                   <*> (comma *> termParser) <* rpar)
    <|> try (S.If <$> (kw "if" *> termParser)
                  <*> (kw "then" *> termParser)
                  <*> (kw "else" *> termParser) <* kw "fi")
    <|> try (S.Let <$> (kw "let" *> identifier)
                   <*> (equal *> termParser)
                   <*> (kw "in" *> termParser) <* kw "end")
    <|> try (S.PrimApp <$> primOp
                       <*> (lpar *> termParser `sepBy1` comma) <* rpar)
    <|> try (S.Project <$> (kw "project" *> lpar *> termParser)
                       <*> (fullstop *> identifier) <* rpar)
    <|> try (S.Record <$> (kw "record" *> tuple recordInternal))
    <|> try (S.Tag <$> (kw "tag" *> lpar *> label)
                   <*> (equal *> termParser)
                   <*> (kw "as" *> typeParser) <* rpar)
    <|> try (S.Case <$> (kw "case" *> termParser)
                    <*> (kw "of" *> caseOptions) <* kw "esac")
    <|> try (lpar *> termParser <* rpar)

parseFile :: String -> IO String
parseFile fname = do
                  inh <- openFile fname ReadMode
                  file_data <- hGetContents inh
                  let x = case parse programParser "" file_data of
                        Left err            -> error (show err)
                        Right parsedProgram -> parsedProgram
                  putStrLn ("GIVEN PROGRAM: " ++ show x)
                  putStrLn ("Free Variables: " ++ show (S.fv x))
                  putStrLn ("Typechecker: " ++ show (T.typeCheck x))
                  let evalOfX = SOS.eval x
                  putStrLn ("Evaluator: " ++ show evalOfX)
                  return $ show evalOfX

testParse :: String -> IO ()
testParse s = putStrLn $ case parse (termParser <* eof) "" s of
        Left err -> "!!! ERROR !!! \n" ++ show err
        Right x  -> show x

