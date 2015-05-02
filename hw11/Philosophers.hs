-- (3.5 балла)

import System.Random
import Control.Monad
import Control.Concurrent.STM
import Control.Concurrent
import System.Environment
{-
Решите задачу о философах http://en.wikipedia.org/wiki/Dining_philosophers_problem
Количество философов передается в параметрах командной строки.
Жизненый цикл философа:
a) Философ сообщает (вывод сообщения на экран) о том, что он готов обедать.
b) Ждет пока не освободятся обе вилки.
c) Берет вилки, сообщает об этом, начинает обедать, что занимает рандомное время (от 1 до 3 секунд).
d) Кладет вилки, сообщает об этом, начинает думать, что занимает рандомное время (от 1 до 3 секунд).
e) Возвращается к шагу (a).

Для реализации используйте библиотеку STM.
Вам также понадобятся функции forkIO, threadDelay и randomRIO.
-}

type Mutex = TVar Bool

tryLock :: Mutex -> STM ()
tryLock l = readTVar l >>= handle where
	handle False = writeTVar l True
	handle _ = retry

unlock :: Mutex -> STM ()
unlock l = writeTVar l False

sleep :: IO ()
sleep = randomRIO (1, 3) >>= threadDelay . (* 10^6)

putThreadInfo :: Int -> String -> IO ()
putThreadInfo i s = putStrLn $ "Thread: " ++ show i ++ ", state: " ++ s

startThread :: Int -> Mutex -> Mutex -> IO ()
startThread i l l' = forever $ do
	putThreadInfo i "READY"
	atomically $ tryLock l >> tryLock l'
	putThreadInfo i "BUSY"
	sleep
	atomically $ unlock l' >> unlock l
	putThreadInfo i "SLEEPING"
	sleep

main :: IO ()
main = do
	count <- fmap (read . head) getArgs
	locks <- replicateM count (newTVarIO False)
	forM_ [0..count - 1] (\ i -> let i' = i + 1 in forkIO $ startThread i' (locks !! i) (locks !! (i' `mod` count)))
	forever $ sleep