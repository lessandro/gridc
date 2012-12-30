{-# LANGUAGE TemplateHaskell, Rank2Types, NoMonomorphismRestriction #-}

module GridC.Codegen (codegen) where

import Control.Lens (makeLenses, (.=), (^.), (%=), (+=), use)
import Control.Monad (liftM)
import Control.Monad.State (State, evalState, get)
import Data.Char (toUpper)
import Data.List (elemIndex)
import Data.Maybe (fromMaybe)

import GridC.AST

type Code = String

data GenState = GenState {
    _locals :: [Identifier],
    _jump :: Int
}

makeLenses ''GenState

type Generator = State GenState [Code]

ops0 :: [String]
ops0 = words "print panic"

ops1 :: [String]
ops1 = words "plus add minus sub mul div min max greater less equal"

concatMapM :: (Monad m) => (a -> m [b]) -> [a] -> m [b]
concatMapM f xs = liftM concat (mapM f xs)

codegen :: Program -> [Code]
codegen program = evalState (genProgram program) emptyState
    where
        emptyState = GenState { _locals = [], _jump = 0 }

genProgram :: Program -> Generator
genProgram (Program functions) = do
    let
        begin = ["CALL @main", "PUSH @end", "GOTO", ""]
        end = ["@end", "END"]

    code <- concatMapM genFunction functions
    return $ begin ++ code ++ end

genFunction :: Function -> Generator
genFunction function = do
    let
        decl = '@' : funcName function
        ret = ["# end " ++ decl, ""]

    locals .= funcArgs function
    body <- genBody (funcBody function)
    return $ decl : body ++ ret

genBody :: [Statement] -> Generator
genBody statements = do
    s <- get
    code <- concatMapM genStatement statements
    locals .= s^.locals
    return code

genStatement :: Statement -> Generator
genStatement (ReturnStm expression) = do
    s <- get
    code <- genExpression expression

    let
        saveRet = ["STORE retval"]
        comment = "# popping locals " ++ show (s^.locals)
        popLocals = comment : map (const "POP") (s^.locals)
        pushRet = ["PUSH retval"]
        ret = ["RETURN"]

    return $ code ++ saveRet ++ popLocals ++ pushRet ++ ret

genStatement (AssignmentStm (Assignment var expression)) = do
    code <- genExpression expression
    locals %= ((++ [var]) . init)
    return $ ("# assign " ++ var) : code

genStatement (ExpressionStm expression) = do
    code <- genExpression expression
    locals %= init
    return $ code ++ ["POP"]

genStatement (IfStm (If condition thenBody elseBody)) = do
    let
        incJump = do
            val <- use jump
            jump += 1
            return val
    
    condCode <- genExpression condition
    locals %= init
    thenCode <- genBody thenBody
    elseJump <- incJump
    elseCode <- genBody elseBody
    endJump <- incJump

    let
        elseLabel = "@L" ++ show elseJump
        endLabel = "@L" ++ show endJump
        check = ["IFFGOTO " ++ elseLabel]
        middle = ["GOTO << " ++ endLabel, elseLabel]
        end = [endLabel]

    return $ condCode ++ check ++ thenCode ++ middle ++ elseCode ++ end

genCall :: Identifier -> [Code]
genCall name
    | name `elem` ops0 = [upperName, "PUSH 0"]
    | name `elem` ops1 = [upperName]
    | otherwise = ["CALL @" ++ name]
    where upperName = map toUpper name

genExpression :: Expression -> Generator
genExpression (ValueExp value) = do
    locals %= (++ [value])
    return ["PUSH " ++ value]

genExpression (FunctionCallExp (FunctionCall name argNames)) = do
    args <- concatMapM genExpression argNames
    locals %= (reverse . drop (length argNames) . reverse)
    locals %= (++ [name ++ " retval"])
    return $ ["# call " ++ name] ++ args ++ genCall name

genExpression (IdentifierExp identifier) = do
    s <- get

    let
        msg = "identifier " ++ identifier ++ " not in scope"
        index = fromMaybe (error msg) (elemIndex identifier $ s^.locals)
        ll = length $ s^.locals

    locals %= (++ ["temp " ++ identifier])
    return ["# " ++ show (s^.locals) ++ " " ++ identifier, "PEEK << " ++ show (index - ll)]
