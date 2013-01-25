module GridC.Parser where

import Control.Applicative ((<$>), (<$), (<*>), (<*))
import Text.Parsec (parse, many, (<|>))
import Text.Parsec.Expr (buildExpressionParser, Operator(..), Assoc(..))

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

        expressionP = buildExpressionParser table termP

        termP = parens expressionP
            <|> Name <$> identifier
            <|> Value . show <$> integer

        table = [
            [Postfix callP, Postfix arrayP],
            [binary "*" "mul", binary "/" "div", binary "%" "mod"],
            [binary "+" "add", binary "-" "sub"],
            [Infix assignP AssocRight]]

        callP = do
            args <- parens (commaSep expressionP)
            return $ flip Call args

        arrayP = do
            index <- brackets expressionP
            return $ flip ArrayAccess index

        assignP = do
            reservedOp "="
            return Assignment

        binary op fun = Infix (opP op fun) AssocLeft

        opP op fun = do
            reservedOp op
            return $ \a b -> Call (Name fun) [a, b]
