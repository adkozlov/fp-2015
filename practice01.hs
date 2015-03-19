dist :: (Double, Double) -> (Double, Double) -> Double
dist (x1, y1) (x2, y2) = sqrt ((x1 - x2)^2 + (y1 - y2)^2)

myIf :: Bool -> a -> a -> a
myIf b x y | b = x
           | otherwise = y

sgn :: Integer -> Int
sgn x | x > 0 = 1
      | x < 0 = -1
      | otherwise = 0

composition :: (b -> c) -> (a -> b) -> (a -> c)
composition f g = \x -> f $ g x

composition' :: (b -> c) -> (a -> b) -> (a -> c)
composition' f g x = f $ g x

(&&&) :: Int -> Int
(&&&) x = x

fib :: Integer -> Integer
fib n = snd $ fib' (1, 1) n
        where fib' (x, y) n | n > 1 = fib' (y, x + y) (n - 1)
                            | otherwise = (x, y)

fib' :: Integer -> Integer
fib' n = let fib'' (x, y) n | n > 1 = fib'' (y, x + y) (n - 1)
                            | otherwise = (x, y)
         in snd $ fib'' (1, 1) n

rec :: a -> (Integer -> a -> a) -> Integer -> a
rec z s 0 = z
rec z s n = s (n - 1) (rec z s (n - 1))

recFac :: Integer -> Integer
recFac = rec 1 (\n -> (* (n + 1)))

fix :: (a -> a) -> a
fix f = f (fix f)

fixFac :: Integer -> Integer
fixFac = fix $ \f n -> if n == 0 then 1 else n * (f (n - 1))
