import Network
import System.IO
import Control.Monad
import Control.Concurrent(forkIO)

{-
Реализуйте простой эхо-сервер.
Это программа, которая слушает определенный порт, принимает соединения, и всё, что присылает клиент, отправляет ему обратно.
(2 балла)
-}

run :: Handle -> IO ()
run h = do
	l <- hGetLine h
	hPutStrLn h l
	hFlush h
	run h

handleConnection :: Socket -> IO ()
handleConnection s = do
	h <- liftM (\ (f, _, _) -> f) $ accept s
	forkIO $ run h
	handleConnection s

main :: IO ()
main = withSocketsDo $ listenOn (PortNumber 12345) >>= handleConnection
