module GridC.Parser (parseGC) where

import Control.Applicative hiding (many, (<|>))
import Text.ParserCombinators.Parsec

import GridC.AST

parseGC :: String -> Program
parseGC input =
    case parse programP "" input of
        Left e -> error $ showError input e
        Right program -> program
    where
        -- program
        programP = Program <$> (spaces *> many (functionP <* spaces)) <* eof

        -- function
        functionP = Function <$> dataTypeP <* spaces <*> identifierP <* spaces <*> funcArgsP <* spaces <*> funcBodyP
        funcArgsP = between (char '(' <* spaces) (char ')') $ sepBy (identifierP <* spaces) (char ',' <* spaces)
        funcBodyP = between (char '{' <* spaces) (char '}') $ many statementP

        -- statement
        statementP =
                IfStm <$> ifP
            <|> statementSMP <* spaces <* char ';' <* spaces

        statementSMP =
                try (ReturnStm <$> returnP)
            <|> try (AssignmentStm <$> assignmentP)
            <|> ExpressionStm <$> expressionP

        -- return
        returnP = string "return" *> spaces *> expressionP

        -- if
        ifP = If <$> (string "if" *> spaces *> ifCondP) <* spaces <*> funcBodyP <* spaces <*> ifElseP
        ifCondP = between (char '(') (char ')') $ between spaces spaces expressionP
        ifElseP = option [] $ string "else" *> spaces *> funcBodyP <* spaces

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

showError :: String -> ParseError -> String
showError src e = init $ unlines [show e, line0, line1, column]
    where
        line0 = if 0 > lineNum - 2 then "" else lines src !! (lineNum - 2)
        line1 = lines src !! (lineNum - 1)
        lineNum = sourceLine $ errorPos e
        column = replicate (colNum - 1) ' ' ++ "^"
        colNum = sourceColumn $ errorPos e
