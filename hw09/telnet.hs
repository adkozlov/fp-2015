import Network
import System.IO
import Control.Concurrent(forkIO)

{-
Реализуйте простой телнет-клиент.
Это программа, которая коннектится по указанному порту и отправляет весь ввод с stdin, а ответ выводит в stdout.
(2 балла)
-}

run :: Handle -> IO ()
run h = hGetLine h >>= putStrLn


handleConnection :: Handle -> IO ()
handleConnection h = do
	l <- getLine
	hPutStrLn h l
	forkIO $ run h
	handleConnection h


main :: IO ()
main = withSocketsDo $ connectTo "127.0.0.1" (PortNumber 12345) >>= handleConnection