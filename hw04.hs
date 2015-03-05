-- 1. fib n вовзращает n-ое число Фибоначчи.
--    Функция должна работать за линейное вермя и определена для всех целых n.
--    Для отрицательных n значение определяется по формуле fib n = fib (n + 2) - fib (n + 1).
--    (1 балл)
fib :: Integer -> Integer
fib = undefined

-- 2a. Написать функцию, возвращающую количество цифр числа.
--     Для целочисленного деления можете использовать функции div и mod.
--    (0.5 балла)
numberOfDigits :: Integer -> Integer
numberOfDigits = undefined

-- 2b. Написать функцию, возвращающую сумму цифр числа.
--    (0.5 балла)
sumOfDigits :: Integer -> Integer
sumOfDigits = undefined

-- 3. gcd' возвращает НОД.
--    (1 балл)
gcd' :: Integer -> Integer -> Integer
gcd' = undefined

-- 4. minp p возвращает минимальное по модулю число x такое, что p x == True. Если такого x не существует, minp не завершается.
--    (1 балл)
minp :: (Integer -> Bool) -> Integer
minp = undefined

-- 5. integral f a b возвращает значение определенного интеграла функции f на отрезке [a,b].
--    Для реализации можете использовать метод трапеций.
--    (2 балла)
integral :: (Double -> Double) -> Double -> Double -> Double
integral = undefined

-- 6. Реализуйте оператор примитивной рекурсии rec, используя функцию (-), укажите тип rec.
--    (1 балл)
rec = undefined

-- 7. Реализуйте факторил при помощи rec.
--    (1 балл)
facRec :: Integer -> Integer
facRec = undefined

-- 8. Реализуйте факториал при помощи fix.
--    (1 балл)
facFix :: Integer -> Integer
facFix = undefined
  where fix f = f (fix f)
