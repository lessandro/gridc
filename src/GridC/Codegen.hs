{-# LANGUAGE Rank2Types, NoMonomorphismRestriction #-}

module GridC.Codegen (codegen) where

import Control.Applicative ((<$>))
import Control.Lens ((.=), (%=), (+=), use)
import Control.Monad (mplus)
import Control.Monad.State (State, execState)
import Data.Char (toUpper)

import GridC.AST
import GridC.Optimizer
import GridC.Util

type Code = String

data Variable = Variable DataType String
    deriving (Show)

data GenState = GenState {
    _locals  :: [Variable],
    _globals :: [Variable],
    _jump    :: Int,
    _code    :: [Code],
    _errors  :: [String]
}

type Generator = State GenState ()

-- define lenses

type Lens a = Functor f => (a -> f a) -> GenState -> f GenState

locals :: Lens [Variable]
locals f s = fmap (\val -> s { _locals = val }) $ f $ _locals s

globals :: Lens [Variable]
globals f s = fmap (\val -> s { _globals = val }) $ f $ _globals s

jump :: Lens Int
jump f s = fmap (\val -> s { _jump = val }) $ f $ _jump s

code :: Lens [Code]
code f s = fmap (\val -> s { _code = val }) $ f $ _code s

errors :: Lens [String]
errors f s = fmap (\val -> s { _errors = val }) $ f $ _errors s

-- lens helpers

infix 4 ++=
(++=) :: Lens [a] -> [a] -> Generator
lens ++= value = lens %= (++ value)

newLocal :: String -> Generator
newLocal name = locals ++= [Variable ValueType name]

newLabel :: State GenState String
newLabel = do
    val <- use jump
    jump += 1
    return $ "@L" ++ show val

-- opcodes that return 0 values
ops0 :: [String]
ops0 = words "print panic exit"

-- opcodes that return 1 value
ops1 :: [String]
ops1 = words a ++ b
    where
        a = "min max abs neg rand"
        b = map snd $ concat binOps

codegen :: Program -> Either String String
codegen program
    | null errors' = Right $ unlines $ optimize code'
    | otherwise = Left $ unlines errors'
    where
        emptyState = GenState {
            _locals = [],
            _globals = [],
            _jump = 0,
            _code = [],
            _errors = []
        }
        state = execState (genProgram program) emptyState
        errors' = _errors state
        code' = _code state

genProgram :: Program -> Generator
genProgram (Program topLevels) = do
    mapM_ genTopLevel topLevels
    functions <- use code
    code .= []
    allocGlobals
    code ++= ["CALL << @main", "END", ""]
    code ++= functions

allocGlobals :: Generator
allocGlobals = do
    allGlobals <- use globals
    let n = sum $ map variableSize allGlobals
    case n of
        0 -> return ()
        _ -> code ++= ["PUSH 0", "DUPN << " ++ show n, ""]

typeSize :: DataType -> Integer
typeSize ValueType = 1
typeSize (ArrayType n) = n

--variable :: Declaration -> Variable
variable :: TopLevel -> Variable
variable (Declaration dataType name) =
    Variable dataType name

variable (Function dataType name _ _) =
    Variable dataType name

variableSize :: Variable -> Integer
variableSize (Variable dataType _) = typeSize dataType

genTopLevel :: TopLevel -> Generator
genTopLevel declaration@(Declaration _ name) = do
    mloc <- findGlobal name
    case mloc of
        Nothing -> globals %= (++ [variable declaration])
        _ -> errors ++= ["global " ++ name ++ " redeclared"]

genTopLevel (Function _ name args statements) = do
    code ++= ['@' : name]
    locals .= map (Variable ValueType) args
    genBody $ addReturn statements
    code ++= ["# end " ++ name, ""]

addReturn :: [Statement] -> [Statement]
addReturn statements = statements ++ [return0 | noReturn]
    where
        return0 = Return (Value "0")
        noReturn
            | null statements = True
            | otherwise = case last statements of
                Return _ -> False
                _ -> True

genBody :: [Statement] -> Generator
genBody statements = do
    oldLocals <- use locals
    mapM_ genStatement statements
    newLocals <- use locals
    locals .= oldLocals
    let diff = drop (length oldLocals) newLocals
    code ++= ["POPN << " ++ show (length diff)]

genStatement :: Statement -> Generator
genStatement (Return expression) = do
    code ++= ["# return"]
    allLocals <- use locals
    genExpression expression
    code ++= ["STORE retval"]
    code ++= ["POPN << " ++ show (length allLocals)]
    code ++= ["PUSH retval", "RETURN"]
    locals %= init

genStatement (ExpressionStm (Assignment (Name name) expression)) = do
    mpos <- findName' name
    genExpression expression
    locals %= init
    case mpos of
        Just pos -> code ++= ["POKE << " ++ show pos]
        Nothing -> newLocal name

genStatement (ExpressionStm expression) = do
    genExpression expression
    code ++= ["POP"]
    locals %= init

genStatement (If condition thenBody elseBody) = do
    elseLabel <- newLabel
    endLabel <- newLabel
    genExpression condition
    code ++= ["IFFGOTO << " ++ elseLabel]
    locals %= init
    genStatement thenBody
    code ++= ["GOTO << " ++ endLabel, elseLabel]
    genStatement elseBody
    code ++= [endLabel]

genStatement (While condition body) = do
    topLabel <- newLabel
    endLabel <- newLabel
    code ++= [topLabel]
    genExpression condition
    code ++= ["IFFGOTO << " ++ endLabel]
    locals %= init
    genStatement body
    code ++= ["GOTO << " ++ topLabel, endLabel]

genStatement (Block statements) = genBody statements

genCall :: Identifier -> Int -> [Code]
genCall name arity
    | name `elem` ops0 = [upperName, "PUSH 0"]
    | name `elem` ops1 = [upperName]
    | name == "ffi" = ["CALLFF << " ++ show arity]
    | otherwise = ["CALL << @" ++ name]
    where upperName = map toUpper name

genExpression :: Expression -> Generator
genExpression (Value value) = do
    newLocal value
    code ++= ["PUSH " ++ value]

genExpression (Call (Name name) argExps) = do
    code ++= ["# call " ++ name]
    oldLocals <- use locals
    mapM_ genExpression argExps
    code ++= genCall name (length argExps)
    locals .= oldLocals
    newLocal $ name ++ " retval"

genExpression Call{} =
    errors ++= ["can't call an arbitrary expression"]

genExpression (Name name@('@':_)) = do
    newLocal $ "temp " ++ name
    code ++= ["PUSH " ++ name]

genExpression (Name name) = do
    pos <- findName name
    newLocal $ "temp " ++ name
    code ++= ["PEEK << " ++ show pos]

genExpression (ArrayAccess (Name name) index) = do
    loc <- findName name
    genExpression index
    newLocal $ "temp " ++ name ++ "[exp]"
    code ++= ["ADD << " ++ show loc, "PEEK"]
    locals %= init

genExpression ArrayAccess{} =
    errors ++= ["can't array-access an arbitrary expression"]

genExpression (Assignment (Name name) expression) = do
    pos <- findName name
    genExpression expression
    code ++= ["DUP"]
    code ++= ["POKE << " ++ show pos]
    locals %= init
    newLocal $ "temp " ++ name

genExpression (Assignment (ArrayAccess (Name name) index) expression) = do
    loc <- findName name
    genExpression expression
    newLocal "tmp array assign"
    code ++= ["DUP"]
    genExpression index
    code ++= ["ADD << " ++ show loc, "POKE"]
    locals %= init . init

genExpression Assignment{} =
    errors ++= ["can't array-assign an arbitrary expression"]

findVariable :: Identifier -> [Variable] -> Maybe Integer
findVariable _ [] = Nothing
findVariable name (x@(Variable _ xname):xs)
    | name == xname = Just 0
    | otherwise = (+ variableSize x) <$> findVariable name xs

findLocal :: Identifier -> State GenState (Maybe Integer)
findLocal name = do
    allLocals <- use locals
    return $ flip (-) (sum $ map variableSize allLocals) <$> findVariable name allLocals

findGlobal :: Identifier -> State GenState (Maybe Integer)
findGlobal name = do
    allGlobals <- use globals
    return $ findVariable name allGlobals

findName' :: Identifier -> State GenState (Maybe Integer)
findName' name = do
    local <- findLocal name
    global <- findGlobal name
    let location = local `mplus` global
    return location

findName :: Identifier -> State GenState Integer
findName name = do
    mloc <- findName' name
    case mloc of
        Just loc -> return loc
        Nothing -> do
            errors ++= ["identifier " ++ name ++ " not in scope"]
            return 9999999
