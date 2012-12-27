module GridC.Codegen (codegen) where

import Control.Monad (liftM)
import Control.Monad.State (State, evalState)
import GridC.AST

type Code = String

data GenState = GenState {
    locals :: [Identifier],
    jumps :: Int
}

type Generator = State GenState [Code]

concatMapM :: (Monad m) => (a -> m [b]) -> [a] -> m [b]
concatMapM f xs = liftM concat (mapM f xs)

codegen :: Program -> [Code]
codegen program = evalState (genProgram program) GenState { locals = [], jumps = 0 }

genProgram :: Program -> Generator
genProgram (Program functions) = do
    let
        begin = ["PUSH @end", "PUSH @main", "GOTO", ""]
        end = ["@end", "END"]

    code <- concatMapM genFunction functions

    return $ begin ++ code ++ end

genFunction :: Function -> Generator
genFunction _ = return []

{-
genFunction function = decl : body ++ ret
    where
        decl = '@' : funcName function
        body = genBody (funcArgs function) (funcBody function)
        ret = ["# end " ++ decl, "GOTO", ""]

genBody :: [Statement] -> Generator
genBody _ [] = []
genBody locals (statement:rest) = code ++ genBody newLocals rest
    where
        (newLocals, code) = genStatement locals statement

genStatement :: Statement -> Generator
genStatement locals (ReturnStm expression) = ([], code ++ popLocals ++ ret)
    where
        code = genExpression locals expression
        popLocals = concatMap (\l -> ["# popping " ++ l, "SWAP", "POP"]) locals
        ret = ["# return", "SWAP", "GOTO"]

genStatement locals (AssignmentStm assignment) = (newLocals, comment ++ code)
    where
        var = asgnVar assignment
        newLocals = locals ++ [var]
        code = genExpression locals (asgnExp assignment)
        comment = ["# assign " ++ var]

genStatement locals (ExpressionStm expression) = (locals, code ++ discard)
    where
        code = genExpression locals expression
        discard = ["POP"]

genExpression :: Expression -> Generator
genExpression _ (ValueExp value) = ["PUSH " ++ value]

genExpression _ (FunctionCallExp functionCall) = ret ++ args ++ call
    where
        args = concatMap genExpression $ callArgs functionCall
        call = "PUSH $$$"
        ret = []

genExpression _ expression = ["# expression " ++ show expression]
-}
