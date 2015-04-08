-- (2 балла)

import Control.Applicative

data Tree a = Node a [Tree a]

instance Functor Tree where
    fmap f (Node e cs) = Node (f e) $ map (fmap f) cs

instance Applicative Tree where
    pure e = Node e $ repeat $ pure e
    (Node e1 cs1) <*> (Node e2 cs2) = Node (e1 e2) $ case cs1 of
        [] -> []
        otherwise -> case cs2 of
            [] -> []
            otherwise -> map (\ (c1, c2) -> c1 <*> c2) $ zip cs1 cs2
    --(Node e1 []) <*> (Node e2 _) = Node (e1 e2) []
    --(Node e1 _) <*> (Node e2 []) = Node (e1 e2) []
    --(Node e1 cs1) <*> (Node e2 cs2) = Node (e1 e2) $ map (\ (c1, c2) -> c1 <*> c2) $ zip cs1 cs2