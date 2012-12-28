{-# LANGUAGE TemplateHaskell, Rank2Types, NoMonomorphismRestriction #-}

module GridC.Codegen (codegen) where

import Control.Lens (makeLenses, (.=), (^.), (%=))
import Control.Monad (liftM)
import Control.Monad.State (State, evalState, get)
import Data.List (elemIndex)
import Data.Maybe (fromMaybe)

import GridC.AST

type Code = String

data GenState = GenState {
    _locals :: [Identifier]
}

makeLenses ''GenState

type Generator = State GenState [Code]

concatMapM :: (Monad m) => (a -> m [b]) -> [a] -> m [b]
concatMapM f xs = liftM concat (mapM f xs)

codegen :: Program -> [Code]
codegen program = evalState (genProgram program) emptyState
    where
        emptyState = GenState { _locals = [] }

genProgram :: Program -> Generator
genProgram (Program functions) = do
    let
        begin = [
            "CALL @main", "PUSH @end", "GOTO", "",
            "@print", "PRINT", "PUSH 0", "RETURN", "",
            "@add", "ADD", "RETURN", ""]
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
genBody = concatMapM genStatement

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

genStatement (AssignmentStm assignment) = do
    let
        var = asgnVar assignment

    code <- genExpression (asgnExp assignment)
    locals %= ((++ [var]) . init)
    return $ ("# assign " ++ var) : code

genStatement (ExpressionStm expression) = do
    code <- genExpression expression
    locals %= init
    return $ code ++ ["POP"]

genExpression :: Expression -> Generator
genExpression (ValueExp value) = do
    locals %= (++ [value])
    return ["PUSH " ++ value]

genExpression (FunctionCallExp functionCall) = do
    let 
        name = callName functionCall
        call = ["CALL @" ++ name]
        argNames = callArgs functionCall

    args <- concatMapM genExpression argNames
    locals %= (reverse . drop (length argNames) . reverse)
    locals %= (++ [name ++ " retval"])
    return $ ["# call " ++ name] ++ args ++ call

genExpression (IdentifierExp identifier) = do
    s <- get

    let
        msg = "identifier " ++ identifier ++ " not in scope"
        index = fromMaybe (error msg) (elemIndex identifier $ s^.locals)
        ll = length $ s^.locals

    locals %= (++ ["temp " ++ identifier])
    return ["# " ++ show (s^.locals) ++ " " ++ identifier, "PEEK << " ++ show (index - ll)]
