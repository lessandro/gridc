module Main (main) where

import System.Environment   

import GridC.Parser

main :: IO ()
main = do
    args <- getArgs
    contents <- readFile $ head args
    print $ parseGC contents
