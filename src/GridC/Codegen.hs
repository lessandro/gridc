{-# LANGUAGE Rank2Types, NoMonomorphismRestriction #-}

module GridC.Codegen (codegen) where

import Control.Applicative ((<$>))
import Control.Lens ((.=), (%=), (+=), use)
import Control.Monad (mplus)
import Control.Monad.State.Strict (State, evalState)
import Data.Char (toUpper)

import GridC.AST
import GridC.Optimizer
import GridC.Util

type Code = String

data Variable = Variable DataType String
    deriving (Show)

data GenState = GenState {
    _locals :: [Variable],
    _globals :: [Variable],
    _jump :: Int
}

type Generator = State GenState [Code]

-- define lenses

locals :: Functor f => ([Variable] -> f [Variable]) -> GenState -> f GenState
locals f (GenState a b c) = fmap (\a' -> GenState a' b c) (f a)

globals :: Functor f => ([Variable] -> f [Variable]) -> GenState -> f GenState
globals f (GenState a b c) = fmap (\b' -> GenState a b' c) (f b)

jump :: Functor f => (Int -> f Int) -> GenState -> f GenState
jump f (GenState a b c) = fmap (GenState a b) (f c)

--

newLocal :: String -> State GenState ()
newLocal name =
    locals %= (++ [Variable ValueType name])

newLabel :: State GenState String
newLabel = do
    val <- use jump
    jump += 1
    return $ "@L" ++ show val

-- opcodes that return 0 values
ops0 :: [String]
ops0 = words "print panic"

-- opcodes that return 1 value
ops1 :: [String]
ops1 = words $ a ++ b
    where
        a = "add sub mul div mod abs neg min max "
        b = "greater less equal nequal rand"

codegen :: Program -> String
codegen program = unlines (optimize compiled)
    where
        compiled = evalState (genProgram program) emptyState
        emptyState = GenState {
            _locals = [],
            _globals = [],
            _jump = 0
        }

genProgram :: Program -> Generator
genProgram (Program topLevels) = do
    let
        begin = ["CALL << @main", "END", ""]

    code <- concatMapM genTopLevel topLevels
    alloc <- allocGlobals
    return $ alloc ++ begin ++ code

allocGlobals :: Generator
allocGlobals = do
    allGlobals <- use globals
    let n = sum $ map variableSize allGlobals
    case n of
        0 -> return []
        _ -> return ["PUSH 0", "DUPN << " ++ show n, ""]

typeSize :: DataType -> Int
typeSize ValueType = 1
typeSize (ArrayType n) = n

--variable :: Declaration -> Variable
variable :: TopLevel -> Variable
variable (Declaration dataType name) =
    Variable dataType name

variable (Function dataType name _ _) =
    Variable dataType name

variableSize :: Variable -> Int
variableSize (Variable dataType _) = typeSize dataType

genTopLevel :: TopLevel -> Generator
genTopLevel declaration@(Declaration _ name) = do
    mloc <- findGlobal name
    case mloc of
        Nothing -> do
            globals %= (++ [variable declaration])
            return []
        _ -> error $ "global " ++ name ++ " redeclared"

genTopLevel (Function _ name args statements) = do
    let
        decl = '@' : name
        ret = ["# end " ++ decl, ""]
        statements' = [Return (Value "0") | noReturn]
        noReturn
            | null statements = True
            | otherwise = case last statements of
                Return _ -> False
                _ -> True

    locals .= map (Variable ValueType) args
    body <- genBody $ statements ++ statements'
    return $ decl : body ++ ret

genBody :: [Statement] -> Generator
genBody statements = do
    oldLocals <- use locals
    code <- concatMapM genStatement statements
    newLocals <- use locals
    locals .= oldLocals

    let
        diff = drop (length oldLocals) newLocals
        popLocals = ["POPN << " ++ show (length diff)]

    return $ code ++ popLocals

genStatement :: Statement -> Generator
genStatement (Return expression) = do
    allLocals <- use locals
    code <- genExpression expression
    locals %= init

    let
        comment = ["# return"]
        saveRet = ["STORE retval"]
        popLocals = ["POPN << " ++ show (length allLocals)]
        pushRet = ["PUSH retval"]
        ret = ["RETURN"]

    return $ comment ++ code ++ saveRet ++ popLocals ++ pushRet ++ ret

genStatement (Assignment name expression) = do
    mpos <- findName' name
    code <- genExpression expression
    locals %= init
    case mpos of
        Just pos ->
            return $ code ++ ["PUSH " ++ show pos, "POKE"]
        Nothing -> do
            newLocal name
            return $ ("# assign " ++ name) : code

genStatement (ExpressionStm expression) = do
    code <- genExpression expression
    locals %= init
    return $ code ++ ["POP"]

genStatement (If condition thenBody elseBody) = do
    condCode <- genExpression condition
    locals %= init
    thenCode <- genStatement thenBody
    elseLabel <- newLabel
    elseCode <- genStatement elseBody
    endLabel <- newLabel

    let
        check = ["IFFGOTO << " ++ elseLabel]
        middle = ["GOTO << " ++ endLabel, elseLabel]
        end = [endLabel]

    return $ condCode ++ check ++ thenCode ++ middle ++ elseCode ++ end

genStatement (While condition body) = do
    topLabel <- newLabel
    condCode <- genExpression condition
    locals %= init
    bodyCode <- genStatement body
    endLabel <- newLabel

    let
        check = ["IFFGOTO << " ++ endLabel]
        end = ["GOTO << " ++ topLabel, "PUSH " ++ topLabel, endLabel]

    return $ topLabel : condCode ++ check ++ bodyCode ++ end

genStatement (ArrayAssignment name index expression) = do
    loc <- findName name
    exprCode <- genExpression expression
    indexCode <- genExpression index
    locals %= init . init
    let assignCode = ["ADD << " ++ show loc, "POKE"]
    return $ exprCode ++ indexCode ++ assignCode

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
    return ["PUSH " ++ value]

genExpression (FunctionCall name argExps) = do
    args <- concatMapM genExpression argExps
    locals %= (reverse . drop (length argExps) . reverse)
    newLocal $ name ++ " retval"
    return $ ["# call " ++ name] ++ args ++ genCall name (length argExps)

genExpression (Identifier name) = do
    pos <- findName name
    newLocal $ "temp " ++ name
    return ["PEEK << " ++ show pos]

genExpression (Constant name) = do
    newLocal $ "temp " ++ name
    return ["PUSH " ++ name]

genExpression (ArrayAccess name index) = do
    loc <- findName name
    indexCode <- genExpression index
    locals %= init
    newLocal $ "temp " ++ name ++ "[exp]"
    return $ indexCode ++ ["ADD << " ++ show loc, "PEEK"]

findVariable :: Identifier -> [Variable] -> Maybe Int
findVariable _ [] = Nothing
findVariable name (x@(Variable _ xname):xs)
    | name == xname = Just 0
    | otherwise = (+ variableSize x) <$> findVariable name xs

findLocal :: Identifier -> State GenState (Maybe Int)
findLocal name = do
    allLocals <- use locals
    return $ flip (-) (sum $ map variableSize allLocals) <$> findVariable name allLocals

findGlobal :: Identifier -> State GenState (Maybe Int)
findGlobal name = do
    allGlobals <- use globals
    return $ findVariable name allGlobals

findName' :: Identifier -> State GenState (Maybe Int)
findName' name = do
    local <- findLocal name
    global <- findGlobal name
    let location = local `mplus` global
    return location

findName :: Identifier -> State GenState Int
findName name = do
    mloc <- findName' name
    case mloc of
        Just loc -> return loc
        Nothing -> error $ "identifier " ++ name ++ " not in scope"
