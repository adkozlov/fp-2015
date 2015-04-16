import Control.Monad.State

data Pair a = Pair a a deriving Show

instance Monad Pair where
	return a = Pair a a

	(Pair x y) >>= k = Pair p q where
		Pair p _ = k x
		Pair _ q = k y

doWhile :: Monad m => m Bool -> m ()
doWhile m = do
	b <- m
	if b then doWhile m else return ()

readXChar = doWhile $ fmap (== 'x') getChar