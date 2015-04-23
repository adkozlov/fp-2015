{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Console (putChar',getChar',runConsole,Console) where

import System.IO

newtype Console a = Console (IO a) deriving Monad

runConsole :: Console a -> IO a
runConsole (Console x) = x

getChar' :: Console Char
getChar' = Console getChar

putChar' :: Char -> Console ()
putChar' = Console . putChar

--instance Monad Console where
--	return = Console . return
--	(Console x) >>= f = Console $ do
--		y <- x
--		runConsole (f y)