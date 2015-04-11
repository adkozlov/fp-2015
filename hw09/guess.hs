import System.Random(randomRIO)
import Control.Monad

{-
Реализуйте следующую программу.
Программа загадывает число от 1 до 100, пользователь должен отгадать его.
После каждой попытки программа говорит больше ее число или меньше.
Если пользователь не отгадал за 5 попыток, то проигрыш, иначе победа.
(1.5 балла)
-}

try :: Int -> Int -> IO ()
try r 5 = putStrLn ("You loose, the number is: " ++ show r)
try r i = liftM (read :: String -> Int) getLine >>= \ n -> case compare r n of
	LT -> putStrLn ">" >> try r (i + 1)
	EQ -> putStrLn "You win"
	GT -> putStrLn "<" >> try r (i + 1)

main :: IO ()
main = do	
	r <- randomRIO (1,100) :: IO Int
	putStrLn $ "Try to guess the number"	
	try r 0