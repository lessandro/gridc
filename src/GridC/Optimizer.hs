module GridC.Optimizer (optimize) where

type Optimizer = [String] -> [String]

optimize :: Optimizer
optimize code
    | optimized == code = code
    | otherwise = optimize optimized
    where
        optimized = peek $ popn1 $ popn0 code

popn0 :: Optimizer
popn0 = filter (/= "POPN << 0")

popn1 :: Optimizer
popn1 = map (replace "POPN << 1" "POP")

peek :: Optimizer
peek = map (replace "PEEK << -1" "DUP")

replace :: String -> String -> String -> String
replace match replacement s
    | s == match = replacement
    | otherwise = s
