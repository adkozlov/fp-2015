import Test.HUnit
-- Нужно поставить библиотеку hunit:
-- cabal install hunit

-- 1. fun четные числа в нечетных позициях (нумеруя с 0) умножает на 2, остальные не изменяет.
-- (0.5 балла)
fun :: [Integer] -> [Integer]
fun = undefined

-- 2. Реализовать следующие функции, используя композицию:
-- (1 балл)

-- fa работает как функция notElem. Используйте функцию elem.
fa :: Eq a => a -> [a] -> Bool
fa = undefined

-- fb g x должен возвращать True, если и только если g x четен. Используйте функцию even.
fb :: (Integer -> Integer) -> Integer -> Bool
fb = undefined

-- fc xs возвращает True, если в xs есть хотя бы 1 положительное число, иначе False. Используйте функции filter и null.
fc :: [Integer] -> Bool
fc = undefined

-- fd p xs возвращает количество элементов в xs, не удовлетворяющих предикату p. Используйте функции filter и length.
fd :: (a -> Bool) -> [a] -> Int
fd = undefined

-- fe возвращает сумму первых 10 элементов списка.
fe :: [Integer] -> Integer
fe = undefined

-- ff каждый элемент умножает на 2, потом прибавляет 3 и возвращает произведение всех элементов. Используйте функцию product.
ff :: [Integer] -> Integer
ff = undefined

-- 3. fibs возвращает бесконечный список чисел Фибоначчи.
-- (0.5 балла)
fibs :: [Integer]
fibs = undefined

-- 4. isPrime проверяет простоту числа.
-- (1 балл)
isPrime :: Integer -> Bool
isPrime = undefined

-- primes возвращает бесконечный список простых чисел.
primes :: [Integer]
primes = undefined

-- 5. shiftL переставляет первый элемент в конец списка. Реализуйте эту функцию так, чтобы она проходила по списку только один раз.
-- (1 балл)
shiftL :: [a] -> [a]
shiftL = undefined

-- shiftR переставляет последний элемент в начало. Реализуйте эту функцию так, чтобы она проходила по списку только один раз.
shiftR :: [a] -> [a]
shiftR = undefined

-- 6. swap i j меняет местами i и j элементы.
-- (1 балл)
swap :: Int -> Int -> [a] -> [a]
swap = undefined

-- 7. takeLast n xs возвращает последние n элементов списка xs.
-- (1 балл)
takeLast :: Int -> [a] -> [a]
takeLast = undefined

-- 8. Назовем элементы, которые удовлетворяют предикату p хорошими, остальные плохими.
-- Тогда mapl p f xs выбрасывает плохие элементы, а блоки подряд идущих хороших элементов,
-- которые разделяются плохими, отправляет в функцию f и возвращает список результатов.
-- Заметьте, что в функцию f никогда не передаются пустые списки.
-- (1 балл)
mapl :: (a -> Bool) -> ([a] -> b) -> [a] -> [b]
mapl = undefined

-- 9. Напишите аналоги функций unlines и unwords, используя функцию intercalate.
--    Заметьте, что функция unlines' работает чуть иначе, чем unlines.
-- (0.5 балла)
unlines' :: [String] -> String
unlines' = undefined

unwords' :: [String] -> String
unwords' = undefined

main = fmap (\_ -> ()) $ runTestTT $ test
    $    label "fun"
    [ fun [1,3,6,10,15,21,28,30,60] ~?= [1,3,6,20,15,21,28,60,60]
    , take 11 (fun fibs) ~?= [1,1,2,3,5,16,13,21,34,55,89]
    ] ++ label "fa"
    [ fa 411 [1..] ~?= notElem 411 [1..]
    , fa (undefined :: Bool) [] ~?= notElem (undefined :: Bool) []
    ] ++ label "fb"
    [ all (fb (*2)) [0..100] ~?= True
    , fb (const 101) undefined ~?= False
    ] ++ label "fc"
    [ fc [-100..1] ~?= True
    , fc [0] ~?= False
    ] ++ label "fd"
    [ fd odd (take 100 primes) ~?= 1
    , fd even (take 100 fibs) ~?= 67
    ] ++ label "fe"
    [ fe fibs ~?= 143
    , fe primes ~?= 129
    , fe [] ~?= 0
    ] ++ label "ff"
    [ ff [1,2,3] ~?= 315
    , ff [] ~?= 1
    ] ++ label "fibs"
    [ take 10 fibs ~?= [1,1,2,3,5,8,13,21,34,55]
    , fibs !! 1000 ~?= 70330367711422815821835254877183549770181269836358732742604905087154537118196933579742249494562611733487750449241765991088186363265450223647106012053374121273867339111198139373125598767690091902245245323403501
    ] ++ label "primes"
    [ take 10 primes ~?= [2,3,5,7,11,13,17,19,23,29]
    , primes !! 1000 ~?= 7927
    ] ++ label "shiftL"
    [ shiftL [1..20] ~?= [2..20] ++ [1]
    , shiftL [] ~?= ([] :: [Bool])
    ] ++ label "shiftR"
    [ shiftR [1..20] ~?= 20:[1..19]
    , shiftR [] ~?= ([] :: [Bool])
    ] ++ label "swap"
    [ swap 1 2 [3,4,5,6] ~?= [3,5,4,6]
    , swap 2 0 "abcd" ~?= "cbad"
    , swap 100 7 [1..10] ~?= [1..10]
    ] ++ label "takeLast"
    [ takeLast 5 [1..20] ~?= [16,17,18,19,20]
    , takeLast 5 [1,2,3] ~?= [1,2,3]
    ] ++ label "mapl"
    [ mapl (\x -> x `mod` 7 /= 3) id [1..20] ~?= [[1,2],[4,5,6,7,8,9],[11,12,13,14,15,16],[18,19,20]]
    , mapl (not . isPrime) sum [1..20] ~?= [1,4,6,27,12,45,18,20]
    ] ++ label "unlines"
    [ unlines' ["abc","def","ghi"] ~?= "abc\ndef\nghi"
    , unlines' ["foo","","","bar"] ~?= "foo\n\n\nbar"
    ] ++ label "unwords"
    [ unwords' [] ~?= ""
    , unwords' ["a","b","cde"] ~?= "a b cde"
    ]
  where
    label :: String -> [Test] -> [Test]
    label l = map (\(i,t) -> TestLabel (l ++ " [" ++ show i ++ "]") t) . zip [1..]
