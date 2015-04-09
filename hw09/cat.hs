import Control.Exception(catch)
import System.Environment
import System.IO.Error
import Control.Monad

{-
cat принимает имена файлов и выводит их содержимое на экран.
Если в cat не передаются параметры, то она копирует stdin в stdout.
Если один из файлов не существовует, нужно вывести сообщение об ошибке и продолжить работу.
(1.5 балла)
-}

ioErrorHandler :: String -> IOError -> IO String
ioErrorHandler f _ = putStrLn ("I/O error occurred during reading the file: " ++ f) >> return ""

cat :: [String] -> IO ()
cat [] = forever $ getLine >>= putStrLn
cat l = catFiles l

catFiles :: [String] -> IO ()
catFiles [] = return ()
catFiles (f:fs) = liftM lines (catch (readFile f) $ ioErrorHandler f) >>= mapM_ putStrLn >> catFiles fs

main :: IO ()
main = do
	args <- getArgs
	cat args