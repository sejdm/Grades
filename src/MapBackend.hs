module MapBackend () where

import qualified Data.Map as M

listsToMap :: Ord a => [a] -> [b] -> M.Map a b
listsToMap xs ys = M.fromList $ zip xs ys


