{-# LANGUAGE TemplateHaskell, Rank2Types, NoMonomorphismRestriction #-}

module GridC.Codegen (codegen) where

import Control.Applicative ((<$>))
import Control.Lens (makeLenses, (.=), (%=), (+=), use)
import Control.Monad (liftM)
import Control.Monad.State.Strict (State, evalState)
import Data.Char (toUpper)
import Data.List (elemIndex)

import GridC.AST

type Code = String

data GenState = GenState {
    _locals :: [Identifier],
    _jump :: Int
}

makeLenses ''GenState

newLabel :: State GenState String
newLabel = do
    val <- use jump
    jump += 1
    return $ "@L" ++ show val

type Generator = State GenState [Code]

-- opcodes that return 0 values
ops0 :: [String]
ops0 = words "print panic"

-- opcodes that return 1 value
ops1 :: [String]
ops1 = words $ a ++ b
    where
        a = "add sub mul div mod abs neg min max "
        b = "greater less equal nequal rand"

concatMapM :: (Monad m) => (a -> m [b]) -> [a] -> m [b]
concatMapM f xs = liftM concat (mapM f xs)

codegen :: Program -> String
codegen program = unlines $ evalState (genProgram program) emptyState
    where
        emptyState = GenState { _locals = [], _jump = 0 }

genProgram :: Program -> Generator
genProgram (Program functions) = do
    let
        begin = ["CALL << @main", "END", ""]

    code <- concatMapM genFunction functions
    return $ begin ++ code

genFunction :: Function -> Generator
genFunction function = do
    let
        decl = '@' : funcName function
        ret = ["# end " ++ decl, ""]
        statements = funcBody function
        statements' = [ReturnStm (ValueExp "0") | noReturn]
        noReturn
            | null statements = True
            | otherwise = case last (funcBody function) of
                ReturnStm _ -> False
                _ -> True

    locals .= funcArgs function
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
        comment = ["# scope exit, pop " ++ show diff | not $ null popLocals]
        popLocals = ["POPN << " ++ show (length diff)]

    return $ code ++ comment ++ popLocals

genStatement :: Statement -> Generator
genStatement (ReturnStm expression) = do
    allLocals <- use locals
    code <- genExpression expression

    let
        saveRet = ["STORE retval"]
        comment = "# ret, pop locals " ++ show allLocals
        popLocals = [comment, "POPN << " ++ show (length allLocals)]
        pushRet = ["PUSH retval"]
        ret = ["RETURN"]

    return $ code ++ saveRet ++ popLocals ++ pushRet ++ ret

genStatement (AssignmentStm (Assignment name expression)) = do
    mpos <- findLocal name
    code <- genExpression expression
    locals %= init
    case mpos of
        Just pos ->
            return $ code ++ ["PUSH " ++ show pos, "SWAP", "POKE"]
        Nothing -> do
            locals %= (++ [name])
            return $ ("# assign " ++ name) : code

genStatement (ExpressionStm expression) = do
    code <- genExpression expression
    locals %= init
    return $ code ++ ["# pop expr stm", "POP"]

genStatement (IfStm (If condition thenBody elseBody)) = do
    condCode <- genExpression condition
    locals %= init
    thenCode <- genBody thenBody
    elseLabel <- newLabel
    elseCode <- genBody elseBody
    endLabel <- newLabel

    let
        check = ["IFFGOTO << " ++ elseLabel]
        middle = ["GOTO << " ++ endLabel, elseLabel]
        end = [endLabel]

    return $ condCode ++ check ++ thenCode ++ middle ++ elseCode ++ end

genStatement (WhileStm (While condition body)) = do
    topLabel <- newLabel
    condCode <- genExpression condition
    locals %= init
    bodyCode <- genBody body
    endLabel <- newLabel

    let
        check = ["IFFGOTO << " ++ endLabel]
        end = ["GOTO << " ++ topLabel, "PUSH " ++ topLabel, endLabel]

    return $ [topLabel] ++ condCode ++ check ++ bodyCode ++ end

genCall :: Identifier -> Int -> [Code]
genCall name arity
    | name `elem` ops0 = [upperName, "PUSH 0"]
    | name `elem` ops1 = [upperName]
    | name == "ffi" = ["CALLFF << " ++ show arity]
    | otherwise = ["CALL << @" ++ name]
    where upperName = map toUpper name

genExpression :: Expression -> Generator
genExpression (ValueExp value) = do
    locals %= (++ [value])
    return ["PUSH " ++ value]

genExpression (FunctionCallExp (FunctionCall name argExps)) = do
    args <- concatMapM genExpression argExps
    locals %= (reverse . drop (length argExps) . reverse)
    locals %= (++ [name ++ " retval"])
    return $ ["# call " ++ name] ++ args ++ genCall name (length argExps)

genExpression (IdentifierExp name) = do
    showLocals <- show <$> use locals
    mpos <- findLocal name
    case mpos of
        Just pos -> do
            locals %= (++ ["temp " ++ name])
            return ["# " ++ showLocals, "PEEK << " ++ show pos]
        Nothing -> error $ "identifier " ++ name ++ " not in scope"

genExpression (ConstantExp name) = do
    locals %= (++ ["temp " ++ name])
    return ["PUSH " ++ name]

findLocal :: Identifier -> State GenState (Maybe Int)
findLocal name = do
    allLocals <- use locals
    return $ flip (-) (length allLocals) <$> elemIndex name allLocals
