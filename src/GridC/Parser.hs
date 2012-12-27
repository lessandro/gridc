module GridC.Parser (parseGC) where

import Control.Applicative hiding (many, (<|>))
import Text.ParserCombinators.Parsec

import GridC.AST

parseGC :: String -> Program
parseGC input =
    case parse programP "" input of
        Right program -> program
        Left e -> error $ show e
    where
        -- program
        programP = Program <$> (spaces *> many (functionP <* spaces)) <* eof

        -- function
        functionP = Function <$> dataTypeP <* spaces <*> identifierP <* spaces <*> funcArgsP <* spaces <*> funcBodyP
        funcArgsP = between (char '(' <* spaces) (char ')') $ sepBy (identifierP <* spaces) (char ',' <* spaces)
        funcBodyP = between (char '{' <* spaces) (char '}') $ endBy statementP (spaces *> char ';' <* spaces)

        -- statement
        statementP =
                try (ReturnStm <$> returnP)
            <|> try (AssignmentStm <$> assignmentP)
            <|> ExpressionStm <$> expressionP
        returnP = string "return" *> spaces *> expressionP

        -- expression
        expressionP =
                try (FunctionCallExp <$> functionCallP)
            <|> IdentifierExp <$> identifierP
            <|> ValueExp <$> many1 digit
            <|> between (char '(' <* spaces) (spaces *> char ')') expressionP

        -- assignment
        assignmentP = Assignment <$> identifierP <* spaces <* char '=' <* spaces <*> expressionP

        -- function call
        functionCallP = FunctionCall <$> identifierP <* spaces <*> between (char '(' <* spaces) (spaces *> char ')') callArgsP
        callArgsP = sepBy expressionP (spaces *> char ',' <* spaces)

        -- etc
        dataTypeP =
                IntType <$ string "int"
            <|> FloatType <$ string "float"
        identifierP = (++) <$> many1 letter <*> many alphaNum
