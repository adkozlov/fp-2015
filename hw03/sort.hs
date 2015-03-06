rec :: a -> (Integer -> a -> a) -> Integer -> a
rec z s 0 = z
rec z s n = s (n - 1) $ rec z s $ n - 1

recList :: a -> (b -> [b] -> a -> a) -> [b] -> a
recList n c [] = n
recList n c (x:xs) = c x xs $ recList n c xs

insert :: Ord a => a -> [a] -> [a] -> [a]
insert x _ [] = [x]
insert x _ (y:ys) | x <= y = (x:y:ys)
                  | otherwise = y:(insert x [] ys)

sort :: Ord a => [a] -> [a]
sort = recList [] insert
