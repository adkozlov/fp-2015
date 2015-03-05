-- 1. fib n вовзращает n-ое число Фибоначчи.
--    Функция должна работать за линейное вермя и определена для всех целых n.
--    Для отрицательных n значение определяется по формуле fib n = fib (n + 2) - fib (n + 1).
--    (1 балл)
fib :: Integer -> Integer
fib n = fib' (1, 1) n where
      fib' (x, y) n | n == 0 = x
                    | n > 0 = fib' (y, x + y) (n - 1)
                    | n < 0 = fib' (y - x, x) (n + 1)

-- 2a. Написать функцию, возвращающую количество цифр числа.
--     Для целочисленного деления можете использовать функции div и mod.
--    (0.5 балла)

digits :: Integer -> [Integer]
digits 0 = [0]
digits n = digits' (abs n) where
           digits' 0 = []
           digits' n = (d:(digits' ((n - d) `div` 10))) where
                       d = n `mod` 10

numberOfDigits :: Integer -> Integer
numberOfDigits n = fromIntegral $ length $ digits n

-- 2b. Написать функцию, возвращающую сумму цифр числа.
--    (0.5 балла)
sumOfDigits :: Integer -> Integer
sumOfDigits n = sum $ digits n

-- 3. gcd' возвращает НОД.
--    (1 балл)
gcd' :: Integer -> Integer -> Integer
gcd' x 0 = x
gcd' x y = gcd' y $ x `mod` y

-- 4. minp p возвращает минимальное по модулю число x такое, что p x == True. Если такого x не существует, minp не завершается.
--    (1 балл)
minp :: (Integer -> Bool) -> Integer
minp p = head $ dropWhile (\x -> (not (p x)) && (not (p (-x)))) [0..]

-- 5. integral f a b возвращает значение определенного интеграла функции f на отрезке [a,b].
--    Для реализации можете использовать метод трапеций.
--    (2 балла)
integral :: (Double -> Double) -> Double -> Double -> Double
integral f a b = h * s where
                 n = 10 ^ 6
                 h = (b - a) / n
                 s = sum [f (a + k * h) | k <- [0..n]]

-- 6. Реализуйте оператор примитивной рекурсии rec, используя функцию (-), укажите тип rec.
--    (1 балл)
rec :: a -> (Integer -> a -> a) -> Integer -> a
rec z s 0 = z
rec z s n = s (n - 1) (rec z s (n - 1))

-- 7. Реализуйте факторил при помощи rec.
--    (1 балл)
facRec :: Integer -> Integer
facRec = rec 1 (\n -> (* (n + 1)))

-- 8. Реализуйте факториал при помощи fix.
--    (1 балл)
facFix :: Integer -> Integer
facFix = fix $ \f n -> if n == 0 then 1 else n * (f (n - 1)) where
         fix f = f (fix f)
