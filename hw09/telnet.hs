import Network
import System.IO
import Control.Concurrent(forkIO)

{-
Реализуйте простой телнет-клиент.
Это программа, которая коннектится по указанному порту и отправляет весь ввод с stdin, а ответ выводит в stdout.
(2 балла)
-}

handleConnection :: Handle -> IO ()
handleConnection c = do
	l <- getLine
	hPutStrLn c l
	l <- hGetLine c
	putStrLn l
	handleConnection c


main :: IO ()
main = withSocketsDo $ connectTo "127.0.0.1" (PortNumber 12345) >>= handleConnection