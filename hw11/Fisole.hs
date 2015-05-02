-- (3 балла)

module Fisole
    ( Fisole, runFisole
    , abortF, putCharF, getCharF
    ) where

import System.IO
import System.IO.Error
import Control.Monad

-- Монада для работы с файлом.
data Fisole a = Fisole { run :: Handle -> IO (Either String a) }

-- Второй аргумент - имя файла с которым будут работать функции putCharF и getCharF.
-- Если произошли какие-то ошибки ввода-вывода (т.е. исключения бросались), нужно их поймать и вернуть Left с каким-нибудь сообщением об ошибке.
runFisole :: Fisole a -> String -> IO (Either String a)
runFisole f p = catchIOError (withFile p ReadWriteMode $ run f) $ return . Left . ioeGetErrorString

instance Functor Fisole where
    fmap f (Fisole r) = Fisole $ \ h -> liftM (fmap f) $ r h

instance Monad Fisole where
    return x = Fisole $ \ _ -> return $ Right x
    (Fisole r) >>= k = Fisole $ \ h -> do
    	io <- r h
    	let io' = (either abortF id . fmap k) io
    	run io' h

-- abortF s завершает вычисление, s - сообщение об ошибке.
abortF :: String -> Fisole a
abortF e = Fisole $ \ _ -> return $ Left e

-- putCharF записывает в файл символ.
putCharF :: Char -> Fisole ()
putCharF c = Fisole $ \ h -> liftM Right $ hPutChar h c

-- getCharF считывает символ из файла.
getCharF :: Fisole Char
getCharF = Fisole $ \ h -> liftM Right $ hGetChar h