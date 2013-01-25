module Main (main) where

import System.Environment

import GridC.AST
import GridC.Parser
import GridC.Codegen

main :: IO ()
main = do
    args <- getArgs
    let name = head args
    contents <- readFile name

    case parseGC name contents of
        Left e -> putStrLn e
        Right ast -> do
            putStrLn $ showAST ast
            putStrLn $ codegen ast
