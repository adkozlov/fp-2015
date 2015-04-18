-- (0.5 балла)
module Counter
    ( Counter
    , tick
    , runCounter
    ) where

-- Монада Counter считает количество тиков, т.е. вызовов функции tick
data Counter a = Counter Int a deriving Show

-- Возвращает результат вычислений и количество тиков
runCounter :: Counter a -> (a, Int)
runCounter (Counter i x) = (x, i)

instance Monad Counter where
    return = Counter 0
    (Counter i x) >>= f = case f x of
    	Counter j y -> Counter (i + j) y

tick :: Counter ()
tick = Counter 1 ()