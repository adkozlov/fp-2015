-- (1.5 балла)
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}

module StateIO
    ( StateIO
    , runStateIO, execStateIO, evalStateIO
    ) where

import Control.Monad.State
import Data.IORef

newtype StateIO s a = StateIO { getStateIO :: IORef s -> IO a }

instance Monad (StateIO s) where
    return x = StateIO $ \ _ -> return x
    (StateIO f) >>= k = StateIO $ \ x -> do
    	y <- fmap k $ f x
    	getStateIO y x

instance MonadState s (StateIO s) where
    get = StateIO readIORef
    put x = StateIO $ \ y -> writeIORef y x

runStateIO :: StateIO s a -> s -> IO (a,s)
runStateIO (StateIO f) x = do
	y <- newIORef x
	result <- f y
	value <- readIORef y
	return (result, value)

execStateIO :: StateIO s a -> s -> IO s
execStateIO (StateIO f) x = newIORef x >>= \y -> f y >> readIORef y

evalStateIO :: StateIO s a -> s -> IO a
evalStateIO (StateIO f) x = newIORef x >>= f