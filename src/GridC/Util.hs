module GridC.Util where

eitherMap :: (a -> c) -> (b -> d) -> Either a b -> Either c d
eitherMap a b c = case c of
    Left d -> Left (a d)
    Right e -> Right (b e)

binOps :: [[(String, String)]]
binOps =
    [
        [("*", "mul"), ("/", "div"), ("%", "mod")],
        [("+", "add"), ("-", "sub")],
        [(">", "greater"), ("<", "less")],
        [("==", "equal"), ("!=", "nequal")],
        [("&", "band")],
        [("^", "bxor")],
        [("&&", "and")]
    ]
