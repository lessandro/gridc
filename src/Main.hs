module Main (main) where

import System.Environment   

import GridC.Parser
import GridC.Codegen

uncomment :: String -> String
uncomment (x:y:xs)
    | x == y && y == '/' = ""
    | otherwise = x : uncomment (y : xs)
uncomment xs = xs

preprocess :: String -> String
preprocess = unlines . map uncomment . lines

main :: IO ()
main = do
    args <- getArgs
    contents <- readFile $ head args
    let processed = preprocess contents
    let ast = parseGC processed
    let output = codegen ast
    putStr $ unlines output
