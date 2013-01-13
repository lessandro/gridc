module GridC.Parser (parseGC) where

import Control.Applicative hiding (many, (<|>))
import Text.ParserCombinators.Parsec

import GridC.AST

uncomment :: String -> String
uncomment (x:y:xs)
    | x == y && y == '/' = ""
    | otherwise = x : uncomment (y : xs)
uncomment xs = xs

preprocess :: String -> String
preprocess = unlines . map uncomment . lines

parseGC :: String -> Program
parseGC input =
    case parse programP "" (preprocess input) of
        Left e -> error $ showError input e
        Right program -> program
    where
        -- program
        programP = Program <$> (spaces *> many topLevelP) <* eof

        -- top level
        topLevelP =
            try (TopDeclaration <$> declarationP)
            <|> (TopFunction <$> functionP)

        -- declaration
        declarationP = try arrayDeclarationP <|> valueDeclarationP

        arrayDeclarationP = do
            _ <- typeP
            spaces
            name <- identifierP
            _ <- char '['
            size <- many1 digit
            _ <- string "];"
            spaces
            return $ Declaration (arrayC size) name

        valueDeclarationP = Declaration <$> valueTypeP <* spaces <*> identifierP <* char ';' <* spaces

        -- function
        functionP = Function <$> valueTypeP <* spaces <*> identifierP <*> funcArgsP <* spaces <*> funcBodyP
        funcArgsP = between (char '(') (char ')') $ sepBy identifierP (char ',' <* spaces)
        funcBodyP = between (char '{' <* spaces) (char '}' <* spaces) $ many statementP

        -- statement
        statementP =
                try (IfStm <$> ifP)
            <|> try (WhileStm <$> whileP)
            <|> statementSMP <* char ';' <* spaces

        statementSMP =
                try (ReturnStm <$> returnP)
            <|> try (ArrayAssignmentStm <$> arrayAssignmentP)
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

        -- array
        arrayAssignmentP = ArrayAssignment <$> arrayAccessP <* spaces <* char '=' <* spaces <*> expressionP

        -- expression
        expressionP = eqP

        -- operators
        eqP = opP addP eqP [("equal", "=="), ("nequal", "!="), ("greater", ">"), ("less", "<")]
        addP = opP mulP addP [("add", "+"), ("sub", "-")]
        mulP = opP primaryP mulP [("mul", "*"), ("div", "/"), ("mod", "%")]

        opP next same ops =
                try (opFunc <$> next <*> choice (map opsP ops) <* spaces <*> same)
            <|> next

        opsP (name, op) = string op *> return name

        opFunc expr1 func expr2 = FunctionCallExp $ FunctionCall func [expr1, expr2]

        -- other expressions
        primaryP =
                try (FunctionCallExp <$> functionCallP)
            <|> try (ArrayAccessExp <$> arrayAccessP <* spaces)
            <|> IdentifierExp <$> identifierP <* spaces
            <|> ConstantExp <$> constantP <* spaces
            <|> ValueExp <$> valueP <* spaces
            <|> between (char '(') (char ')' <* spaces) expressionP

        -- array access
        arrayAccessP = ArrayAccess <$> identifierP <* char '[' <*> expressionP <* char ']'

        -- function call
        functionCallP = FunctionCall <$> identifierP <*> callArgsP
        callArgsP = between (char '(') (char ')' <* spaces) exprListP
        exprListP = sepBy expressionP (char ',' <* spaces)

        -- types
        valueTypeP = ValueType <$ typeP

        typeP =
                string "int"
            <|> string "float"
            <|> string "void"

        arrayC = ArrayType . atoi
        atoi s = read s :: Int

        -- etc
        identifierP = (++) <$> many1 letter <*> many (alphaNum <|> char '_')

        constantP = (:) <$> char '@' <*> many (alphaNum <|> char '_')

        valueP = option "" (string "-") `cc` many1 digit `cc` fractionP
        fractionP = option "" $ string "." `cc` many1 digit

        cc = liftA2 (++)

showError :: String -> ParseError -> String
showError src e = init $ unlines [show e, line0, line1, column]
    where
        line0 = if lineNum == 1 then "" else showLine (lineNum - 1)
        line1 = showLine lineNum
        lineNum = sourceLine (errorPos e)
        showLine n = (lines src ++ [""]) !! (n - 1)
        column = replicate (colNum - 1) ' ' ++ "^"
        colNum = sourceColumn (errorPos e)
