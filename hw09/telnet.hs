import Network
import System.IO
import Control.Concurrent(forkIO)

{-
Реализуйте простой телнет-клиент.
Это программа, которая коннектится по указанному порту и отправляет весь ввод с stdin, а ответ выводит в stdout.
(2 балла)
-}

handleConnectionIn :: Handle -> IO ()
handleConnectionIn h = hGetLine h >>= putStrLn >> handleConnectionIn h

handleConnectionOut :: Handle -> IO ()
handleConnectionOut h = getLine >>= hPutStrLn h >> handleConnectionOut h

main :: IO ()
main = withSocketsDo $ do
	h <- connectTo "127.0.0.1" (PortNumber 12345)
	forkIO $ handleConnectionOut h
	handleConnectionIn h