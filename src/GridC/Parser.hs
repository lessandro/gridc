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
        programP = Program <$> (spaces *> many functionP) <* eof

        -- function
        functionP = Function <$> dataTypeP <* spaces <*> identifierP <*> funcArgsP <* spaces <*> funcBodyP
        funcArgsP = between (char '(') (char ')') $ sepBy identifierP (char ',' <* spaces)
        funcBodyP = between (char '{' <* spaces) (char '}' <* spaces) $ many statementP

        -- statement
        statementP =
                IfStm <$> ifP
            <|> WhileStm <$> whileP
            <|> statementSMP <* char ';' <* spaces

        statementSMP =
                try (ReturnStm <$> returnP)
            <|> try (AssignmentStm <$> assignmentP)
            <|> ExpressionStm <$> expressionP

        -- if
        ifP = If <$> (string "if" *> spaces *> condP) <*> funcBodyP <*> ifElseP
        condP = between (char '(') (char ')' <* spaces) expressionP
        ifElseP = option [] $ string "else" *> spaces *> funcBodyP

        -- while
        whileP = While <$> (string "while" *> spaces *> condP) <*> funcBodyP

        -- return
        returnP = string "return" *> spaces *> expressionP

        -- assignment
        assignmentP = Assignment <$> identifierP <* spaces <* char '=' <* spaces <*> expressionP

        -- expression
        expressionP = eqP

        -- operators
        eqP = opP addP eqP [("equal", "==")]
        addP = opP mulP addP [("add", "+"), ("sub", "-")]
        mulP = opP primaryP mulP [("mul", "*"), ("div", "/")]

        opP next same ops =
                try (opFunc <$> next <*> choice (map opsP ops) <* spaces <*> same)
            <|> next

        opsP (name, op) = string op *> return name

        opFunc expr1 func expr2 = FunctionCallExp $ FunctionCall func [expr1, expr2]

        -- other expressions
        primaryP =
                try (FunctionCallExp <$> functionCallP)
            <|> IdentifierExp <$> identifierP <* spaces
            <|> ValueExp <$> valueP <* spaces
            <|> between (char '(') (char ')' <* spaces) expressionP

        -- function call
        functionCallP = FunctionCall <$> identifierP <*> callArgsP
        callArgsP = between (char '(') (char ')' <* spaces) exprListP
        exprListP = sepBy expressionP (char ',' <* spaces)

        -- etc
        dataTypeP =
                IntType <$ string "int"
            <|> FloatType <$ string "float"

        identifierP = (++) <$> many1 letter <*> many alphaNum

        valueP = option "" (string "-") `cc` many1 digit `cc` fractionP
        fractionP = option "" $ string "." `cc` many1 digit

        cc = liftA2 (++)

showError :: String -> ParseError -> String
showError src e = init $ unlines [show e, line0, line1, column]
    where
        line0 = if 0 > lineNum - 2 then "" else lines src !! (lineNum - 2)
        line1 = lines src !! (lineNum - 1)
        lineNum = sourceLine $ errorPos e
        column = replicate (colNum - 1) ' ' ++ "^"
        colNum = sourceColumn $ errorPos e
