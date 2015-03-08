rec :: a -> (Integer -> a -> a) -> Integer -> a
rec z s 0 = z
rec z s n = s (n - 1) $ rec z s $ n - 1

minus :: Integer -> Integer -> Integer
minus n m = if n > m then n - m else 0

false :: Integer -> Bool -> Bool
false n _ = False

isZero :: Integer -> Bool -> Bool
isZero n _ = rec True false n

ge :: Integer -> Integer -> Bool
ge n m  = rec True isZero (minus m n)

multSucc :: Integer -> (Integer -> Integer)
multSucc n = (* (n + 1))

fac :: Integer -> Integer
fac = rec 1 multSucc

ack :: Integer -> Integer -> Integer
ack = rec (+ 1) (\n r -> iter (\ a b -> r b))

iter :: (Integer -> Integer -> Integer) -> Integer -> Integer
iter s n = rec (s 0 1) s n
