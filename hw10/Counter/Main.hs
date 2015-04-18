-- (0.5 балла)
import Counter

-- Эти две функции отличаются от обычных тем, что, вызывая tick, они считают сколько шагов заняло их выполнение.
filter' :: (a -> Bool) -> [a] -> Counter [a]
filter' _ [] = return []
filter' p (x:xs) = do
	tick
	ys <- filter' p xs
	if p x then return (x:ys) else return ys

append :: [a] -> [a] -> Counter [a]
append xs [] = return xs
append xs (y:ys) = do
	tick
	append (xs ++ [y]) ys


-- Реализуйте qsort через filter' и append
qsort :: Ord a => [a] -> Counter [a]
qsort [] = return[]
qsort (x:xs) = do
	let sort = \ p -> filter' p xs >>= qsort
	l <- sort (< x)
	g <- sort (>= x)
	result <- append l [x]
	append result g

-- Первый вызов должен занимать большее количество тиков ~ в 2 раза
main = do
    print $ runCounter $ qsort [1..15]
    print $ runCounter $ qsort [8,4,12,2,6,10,14,1,3,5,7,9,11,13,15]
