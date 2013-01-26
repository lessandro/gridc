module GridC.Parser where

import Control.Applicative ((<$>), (<$), (<*>), (<*), (*>))
import Text.Parsec (parse, option, many, (<|>))
import Text.Parsec.Error (ParseError, errorPos)
import Text.Parsec.Expr (buildExpressionParser, Operator(..), Assoc(..))
import Text.Parsec.Pos (sourceLine, sourceColumn)

import GridC.AST
import GridC.Lexer
import GridC.Util

parseGC :: String -> String -> Either String Program
parseGC programName input = eitherMap (showError input) id parsed
    where
        parsed = parse programP programName input

        programP = Program <$> many topLevelP

        topLevelP = do
            valueType <- valueTypeP
            name <- identifier
            declTypeP valueType name

        declTypeP valueType name =
            Declaration valueType name <$ semi
            <|> (\n -> Declaration (ArrayType n) name) <$> brackets natural <* semi
            <|> Function valueType name <$> funcArgsP <*> funcBodyP

        funcArgsP = parens (commaSep identifier)
        funcBodyP = braces (many statementP)

        valueTypeP = ValueType <$ (reserved "int" <|> reserved "void")

        statementP =
            expressionStmP <* semi
            <|> Return <$> (reserved "return" *> option (Value "0") expressionP) <* semi
            <|> While <$> (reserved "while" *> parens expressionP) <*> statementP
            <|> If <$> (reserved "if" *> parens expressionP) <*> statementP <*> elseP
            <|> forP
            <|> Block <$> funcBodyP

        expressionStmP = ExpressionStm <$> expressionP

        elseP = option (Block []) (reserved "else" *> statementP)

        forP = do
            _ <- reserved "for"
            (first, cond, incr) <- parens forParensP
            body <- statementP
            return $ Block [first, While cond (Block [body, incr])]

        forParensP = do
            first <- option (Block []) expressionStmP <* semi
            cond <- option (Value "1") expressionP <* semi
            incr <- option (Block []) expressionStmP
            return (first, cond, incr)

        expressionP = buildExpressionParser table termP

        termP = parens expressionP
            <|> Name <$> identifier
            <|> Value . show <$> integer

        table = [[Postfix callP, Postfix arrayP]] ++ ops ++ [[Infix assignP AssocRight]]

        callP = do
            args <- parens (commaSep expressionP)
            return $ flip Call args

        arrayP = do
            index <- brackets expressionP
            return $ flip ArrayAccess index

        assignP = do
            reservedOp "="
            return Assignment

        ops = map (map binary) binOps

        binary (op, fun) = Infix (opP op fun) AssocLeft

        opP op fun = do
            reservedOp op
            return $ \a b -> Call (Name fun) [a, b]

showError :: String -> ParseError -> String
showError src e = init $ unlines [show e, line0, line1, column]
    where
        line0 = if lineNum == 1 then "" else showLine (lineNum - 1)
        line1 = showLine lineNum
        lineNum = sourceLine (errorPos e)
        showLine n = (lines src ++ [""]) !! (n - 1)
        column = replicate (colNum - 1) ' ' ++ "^"
        colNum = sourceColumn (errorPos e)
