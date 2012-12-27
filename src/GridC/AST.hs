module GridC.AST where

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
