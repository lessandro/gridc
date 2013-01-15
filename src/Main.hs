module Main (main) where

import System.Environment   

import GridC.AST
import GridC.Parser
import GridC.Codegen

main :: IO ()
main = do
    args <- getArgs
    contents <- readFile $ head args
    let ast = parseGC contents
    putStrLn $ showAST ast ++ "\n"
    let output = codegen ast
    putStr output
