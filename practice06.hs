import System.Environment
import Data.List
import Control.Monad
import System.IO
import Data.IORef

echo = getArgs >>= \ s -> putStrLn $ intercalate " " s
cat = forever $ getLine >>= putStrLn
grep p = forever $ getLine >>= \ t -> if (isInfixOf p t) then (putStrLn t) else return ()

main = hSetBuffering stdout LineBuffering >> grep "a"

fac :: Integer -> IO Integer
fac n = do
    r <- newIORef 1
    forM_ [1..n] $ \ i -> modifyIORef r $ \ x -> x * i
    readIORef r

fix :: (a -> a) -> a
fix f = f (fix f)