module GridC.AST where

import Data.Char

type Identifier = String

data DataType = ValueType | ArrayType Int
    deriving (Show)

data Program = Program [TopLevel]
    deriving (Show)

data TopLevel =
    Declaration DataType Identifier
    | Function DataType Identifier [Identifier] [Statement]
    deriving (Show)

data Statement =
    ExpressionStm Expression
    | Return Expression
    | If Expression Statement Statement
    | While Expression Statement
    | Block [Statement]
    deriving (Show)

data Expression =
    Call Expression [Expression]
    | Assignment Expression Expression
    | ArrayAccess Expression Expression
    | Value String
    | Name Identifier
    deriving (Show)

showAST :: Show a => a -> String
showAST x = '#' : showAST' 0 (show x)

showAST' :: Int -> String -> String
showAST' _ [] = []
showAST' n (x:xs)
    | x `elem` "{[(" = x : indent (n + 2) ++ showAST' (n + 2) rest
    | x `elem` "}])" = indent (n - 2) ++ [x] ++ showAST' (n - 2) rest
    | x == ',' = x : indent n ++ showAST' n rest
    | otherwise = x : showAST' n xs
    where
        indent m = '\n' : '#' : replicate m ' '
        rest = dropWhile isSpace xs
