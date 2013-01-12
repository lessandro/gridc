module GridC.AST where

type Identifier = String

data DataType = ValueType | ArrayType Int
    deriving (Show)

data Program = Program [TopLevel]
    deriving (Show)

data TopLevel =
    TopDeclaration Declaration
    | TopFunction Function
    deriving (Show)

data Declaration = Declaration {
    declType :: DataType,
    declName :: Identifier
}
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
    | ArrayAssignmentStm ArrayAssignment
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
    | ArrayAccessExp ArrayAccess
    | ConstantExp String
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

data ArrayAccess = ArrayAccess {
    arrayName :: Identifier,
    arrayIndex :: Expression
}
    deriving (Show)

data ArrayAssignment = ArrayAssignment {
    aaArray :: ArrayAccess,
    aaExp :: Expression
}
    deriving (Show)
