import Control.Exception(catch)
import System.Environment
import System.IO.Error
import Control.Monad
import Data.List

{-
grep принимает строку и от 0 и больше имен файлов, выводит строки, в которых встречается как подстрока переданная первым параметром строчка.
Если один из файлов не существовует, нужно вывести сообщение об ошибке и продолжить работу.
(1.5 балла)
-}

ioErrorHandler :: String -> IOError -> IO String
ioErrorHandler f _ = putStrLn ("I/O error occurred during reading the file: " ++ f) >> return ""

printIfIsInfixOf :: String -> String -> IO ()
printIfIsInfixOf p t = if (isInfixOf p t) then (putStrLn t) else return ()

grep :: [String] -> IO ()
grep (p:[]) = return ()
grep (p:f:fs) = liftM lines (catch (readFile f) $ ioErrorHandler f) >>= mapM_ (printIfIsInfixOf p) >> grep (p:fs)
grep _ = putStrLn "Usage: grep <pattern> [<filename>...]"

main :: IO ()
main = do
	args <- getArgs
	grep args
