module GridC.Optimizer (optimize) where

type Optimizer = [String] -> [String]

optimize :: Optimizer
optimize code
    | optimized == code = code
    | otherwise = optimize optimized
    where
        optimized = pushes $ gotos $ peek $ popn1 $ popn0 code

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

gotos :: Optimizer
gotos [] = []
gotos (x:xs)
    | isGoto && uselessGoto = continue
    | otherwise = x : continue
    where
        continue = gotos xs
        isGoto = isOp "GOTO" x
        uselessGoto = comesNext (last $ words x) xs

pushes :: Optimizer
pushes (x:y:xs)
    | isPush && isPop = pushes xs
    | otherwise = x : pushes (y:xs)
    where
        isPush = isOp "PUSH" x
        isPop = isOp "POP" y

pushes xs = xs

isOp :: String -> String -> Bool
isOp _ [] = False
isOp op x = head (words x) == op

comesNext :: String -> [String] -> Bool
comesNext _ [] = False
comesNext target (x:xs)
    | x == target = True
    | isNop x = comesNext target xs
    | otherwise = False

isNop :: String -> Bool
isNop [] = True
isNop ('#':_) = True
isNop ('@':_) = True
isNop _ = False
