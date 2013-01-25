module Main (main) where

import System.Environment (getArgs)

import GridC.AST (showAST)
import GridC.Parser (parseGC)
import GridC.Codegen (codegen)

main :: IO ()
main = do
    args <- getArgs
    let name = head args
    contents <- readFile name

    case parseGC name contents of
        Left e -> putStrLn e
        Right ast -> do
            putStrLn $ showAST ast
            case codegen ast of
                Left e -> putStrLn e
                Right code -> putStrLn code
