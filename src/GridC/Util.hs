module GridC.Util where

import Control.Monad (liftM)

concatMapM :: (Monad m) => (a -> m [b]) -> [a] -> m [b]
concatMapM f xs = liftM concat (mapM f xs)

eitherMap :: (a -> c) -> (b -> d) -> Either a b -> Either c d
eitherMap a b c = case c of
    Left d -> Left (a d)
    Right e -> Right (b e)
