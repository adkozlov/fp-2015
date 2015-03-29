module Tree where

-- (2 балла)

data Tree a = Node { value :: a, children :: [Tree a] } deriving Eq

-- show и read должны работать как описано в тестах в Main.hs
instance Show a => Show (Tree a) where
    show (Node v []) = show v
    show (Node v (c:cs)) = show v ++ ":{" ++ show c ++ show' cs ++ "}" where
    	                                               show' [] = ""
    	                                               show' (x:xs) = "," ++ show x ++ show' xs

instance Read a => Read (Tree a) where
    readsPrec = undefined

instance Functor Tree where
    fmap f (Node v c) = Node (f v) $ map (fmap f) c
