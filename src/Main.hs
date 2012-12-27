module Main (main) where

import System.Environment   

import GridC.Parser
import GridC.Codegen

main :: IO ()
main = do
    args <- getArgs
    contents <- readFile $ head args
    let ast = parseGC contents
    print ast
    let output = codegen ast
    putStrLn $ unlines output
