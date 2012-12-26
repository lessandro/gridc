import Control.Applicative hiding (many, (<|>))
import Text.ParserCombinators.Parsec

data DataType = IntType | VoidType
    deriving (Show)

data Program = Program [Function]
    deriving (Show)

data Function = Function {
    funcType :: DataType,
    funcName :: String,
    funcArgs :: [String],
    funcBody :: [Statement]
}
    deriving (Show)

data Statement =
    ExpressionStm Expression
    | ReturnStm Expression
    deriving (Show)

data Expression =
    FunctionCallExp FunctionCall
    | AssignmentExp Assignment
    | ValueExp String
    | IdentifierExp String
    deriving (Show)

data FunctionCall = FunctionCall {
    callName :: String,
    callArgs :: [Expression]
}
    deriving (Show)

data Assignment = Assignment {
    asgnVar :: String,
    asgnExp :: Expression
}
    deriving (Show)

parseGC :: String -> String
parseGC input =
    case parse programP "" input of
        Right program -> show program
        Left e -> show e
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
            <|> ExpressionStm <$> expressionP
        returnP = string "return" *> spaces *> expressionP

        -- expression
        expressionP =
                try (FunctionCallExp <$> functionCallP)
            <|> try (AssignmentExp <$> assignmentP)
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
            <|> VoidType <$ string "void"
        identifierP = (++) <$> many1 letter <*> many alphaNum

main :: IO ()
main = readFile "example.gridc" >>= (putStrLn . parseGC)
