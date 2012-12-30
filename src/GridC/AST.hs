module GridC.AST where

type Identifier = String

data DataType = IntType | FloatType
    deriving (Show)

data Program = Program [Function]
    deriving (Show)

data Function = Function {
    funcType :: DataType,
    funcName :: Identifier,
    funcArgs :: [Identifier],
    funcBody :: [Statement]
}
    deriving (Show)

data Statement =
    ExpressionStm Expression
    | ReturnStm Expression
    | AssignmentStm Assignment
    | IfStm If
    | WhileStm While
    deriving (Show)

data If = If {
    ifCond :: Expression,
    ifThen :: [Statement],
    ifElse :: [Statement]
}
    deriving (Show)

data While = While {
    whileCond :: Expression,
    whileBody :: [Statement]
}
    deriving (Show)

data Expression =
    FunctionCallExp FunctionCall
    | ValueExp String
    | IdentifierExp String
    deriving (Show)

data FunctionCall = FunctionCall {
    callName :: Identifier,
    callArgs :: [Expression]
}
    deriving (Show)

data Assignment = Assignment {
    asgnVar :: Identifier,
    asgnExp :: Expression
}
    deriving (Show)
