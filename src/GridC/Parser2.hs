module GridC.Parser2 where

import Control.Applicative ((<$>), (<$), (<*>), (<*))
import Text.Parsec (parse, try, many, (<|>))

import GridC.AST
import GridC.Lexer
import GridC.Util

parseGC :: String -> String -> Either String Program
parseGC name input = eitherMap show id $ parse programP name input
    where
        programP = Program <$> many functionP

        functionP = Function <$> valueTypeP <*> identifier <*> funcArgsP <*> funcBodyP
        funcArgsP = parens (commaSep identifier)
        funcBodyP = braces (many statementP)

        valueTypeP = ValueType <$ (symbol "int" <|> symbol "void")

        statementP = ExpressionStm <$> expressionP <* semi

        expressionP =
            try functionCallP
            <|> Identifier <$> identifier
            <|> Value . show <$> integer

        functionCallP = FunctionCall <$> identifier <*> parens (commaSep expressionP)
