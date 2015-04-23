-- (3 балла)

module Fisole
    ( Fisole, runFisole
    , abortF, putCharF, getCharF
    ) where

-- Монада для работы с файлом.
data Fisole a = Undefined

-- Второй аргумент - имя файла с которым будут работать функции putCharF и getCharF.
-- Если произошли какие-то ошибки ввода-вывода (т.е. исключения бросались), нужно их поймать и вернуть Left с каким-нибудь сообщением об ошибке.
runFisole :: Fisole a -> String -> IO (Either String a)
runFisole = undefined

instance Functor Fisole where
    fmap = undefined

instance Monad Fisole where
    return = undefined
    (>>=) = undefined

-- abortF s завершает вычисление, s - сообщение об ошибке.
abortF :: String -> Fisole a
abortF = undefined

-- putCharF записывает в файл символ.
putCharF :: Char -> Fisole ()
putCharF = undefined

-- getCharF считывает символ из файла.
getCharF :: Fisole Char
getCharF = undefined
