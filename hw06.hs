import qualified Data.Map as M
import Prelude hiding (lookup)
import Test.HUnit

------------------------------------------------------------------------------
-- 1. Реализуйте функции для работы с комплекснми числами.

data Complex = Complex { real :: Double, im :: Double } deriving (Show, Eq)

fromDouble :: Double -> Complex
fromDouble d = Complex d 0

-- Мнимая единица
i :: Complex
i = Complex 0 1

infixl 6 +., -.
(+.) :: Complex -> Complex -> Complex
(+.) (Complex re1 im1) (Complex re2 im2) = Complex (re1 + re2) (im1 + im2)

(-.) :: Complex -> Complex -> Complex
(-.) (Complex re1 im1) (Complex re2 im2) = Complex (re1 - re2) (im1 - im2)

infixl 7 *., /.
(*.) :: Complex -> Complex -> Complex
(*.) (Complex re1 im1) (Complex re2 im2) = Complex (re1 * re2 - im1 * im2) (re2 * im1 + re1 * im2)

(/.) :: Complex -> Complex -> Complex
(/.) (Complex re1 im1) (Complex re2 im2) = Complex ((re1 * re2 + im1 * im2) / r) ((re2 * im1 - re1 * im2) / r) where r = re2 ** 2 + im2 ** 2

conj :: Complex -> Complex
conj (Complex re im) = Complex re (-im)

-- tests

testsComplex =
    [ i *. i ~?= fromDouble (-1)
    , fromDouble 3 +. i ~?= Complex 3 1
    , fromDouble 3 *. i ~?= Complex 0 3
    , (fromDouble 3 +. fromDouble 4 *. i) *. (fromDouble 4 +. fromDouble 3 *. i) ~?= fromDouble 25 *. i
    , conj (fromDouble 3 +. fromDouble 4 *. i) ~?= fromDouble 3 -. fromDouble 4 *. i
    , fromDouble 2 /. (fromDouble 1 +. i) ~?= fromDouble 1 -. i
    ]

------------------------------------------------------------------------------
-- 2

data Tree a = Node { value :: a, children :: [Tree a] }

-- (a) Возвращает высоту дерева
height :: Tree a -> Int
height (Node _ []) = 1
height (Node _ c) = 1 + (maximum (map height c))

-- (b) Возвращает среднее арифметическое значений во всех узлах дерева
-- Необходимо вычислить эту функцию, выполнив один проход по дереву
avg :: Tree Int -> Int
avg t = (fst result) `div` (snd result) where result = avg' t

avg' :: Tree Int -> (Int, Int)
avg' (Node v []) = (v, 1)
avg' (Node v c) = (v + (sum (map fst result)), 1 + (sum (map snd result))) where result = map avg' c

-- (c) Возвращает ширину дерева
-- Ширина дерева определяется следующим образом:
-- Количество вершин на определенном уровне называется шириной уровня.
-- Ширина дерева - это максимальная ширина уровня по всем уровням.
width :: Tree a -> Int
width = lenOrWidth . children
  where
    lenOrWidth :: [Tree a] -> Int
    lenOrWidth c = max (length c) $ sum $ map (length . children) c

-- tests

(tree1, tree2, tree3) =
    ( b [b [l [b []],
            l [b [],
               l [b [l [],
                     l [],
                     b []],
                  l []]]],
         b [],
         b [],
         l []]
    , b [b [b [],
            b [b [],
               b []]],
         b [b [],
            l [b [],
               b []]],
         l [b []]]
    , b [tree1, tree2]
    )
  where l = Node 500; b = Node 300

(testsHeight, testsAvg, testsWidth) = (
    [ height tree1 ~?= 6
    , height tree2 ~?= 4
    , height tree3 ~?= 7
    ],
    [ avg tree1 ~?= 393
    , avg tree2 ~?= 330
    , avg tree3 ~?= 362
    ],
    [ width tree1 ~?= 4
    , width tree2 ~?= 5
    , width tree3 ~?= 7
    ])

------------------------------------------------------------------------------
-- 3

data Value = I Int | B Bool deriving (Eq, Show)
data BinOp = Plus | Mul | Minus | Less | Greater | Equals
data UnOp = Neg | Not
data Expr = BinOp BinOp Expr Expr | UnOp UnOp Expr | Const Value | If Expr Expr Expr | Var String
data Statement = Assign String Expr | While Expr Statement | Compound [Statement]

infixr 0 @=
(@=) = Assign
(.+) = BinOp Plus
(.-) = BinOp Minus
(.*) = BinOp Mul
(.<) = BinOp Less
int = Const . I
bool = Const . B
neg = UnOp Neg

type Error = String

-- evalExpr m e интерпретирует выражение e, в m передается значение переменных.
-- evalExpr возвращает либо успешно вычисленный результат, либо список ошибок.
-- Ошибки бывают двух видов: необъявленная переменная и несоответствие типов.
-- Возвращается список ошибок, т.к. выражение может содержать больше одной ошибки.
evalExpr :: M.Map String Value -> Expr -> Either [Error] Value
--
-- Зачем такие длинные имена у локальных переменных?
-- И зачем всё писать в одну строчку?
--
evalExpr m (BinOp op e1 e2) = case evalExpr m e1 of
    Left errs -> Left errs
    Right value -> either Left (apply op value) (evalExpr m e2) where
                                apply Plus (I int1) (I int2) = Right $ I $ int1 + int2
                                apply Mul (I int1) (I int2) = Right $ I $ int1 * int2
                                apply Minus (I int1) (I int2) = Right $ I $ int1 - int2
                                apply Less (I int1) (I int2) = Right $ B $ int1 < int2
                                apply Greater (I int1) (I int2) = Right $ B $ int1 > int2
                                apply Equals (I int1) (I int2) = Right $ B $ int1 == int2
                                apply _ _ _ = Left ["type mismatch"]
evalExpr m (UnOp op e) = either Left (apply op) (evalExpr m e) where
                                      apply Neg (I int) = Right $ I $ -int
                                      apply Not (B bool) = Right $ B $ not bool
                                      apply _ _ = Left ["type mismatch"]
evalExpr m (Const value) = Right value
evalExpr m (If cond thenExpr elseExpr) = either Left apply (evalExpr m cond) where
                                                     apply (B True) = evalExpr m thenExpr
                                                     apply (B False) = evalExpr m elseExpr
                                                     apply _ = Left ["type mismatch"]
evalExpr m (Var s) = maybe (Left ["variable is not declared: " ++ string]) Right (M.lookup s m)

-- evalStatement принимает текущее значение переменных и statement и возвращает новое значение переменных после его выполнения.
evalStatement :: M.Map String Value -> Statement -> Either [Error] (M.Map String Value)
evalStatement m (Assign var e) = case evalExpr m e of
    Left errs -> Left errs
    Right value -> Right (M.insert var value m)
evalStatement m e@(While cond statement) = either Left execute (evalExpr m cond) where
                                                       execute (B True) = case evalStatement m statement of
                                                                            Left errs -> Left errs
                                                                            Right m' -> evalStatement m' e
                                                       execute (B False) = Right m
                                                       execute _ = Left ["type mismatch"]
evalStatement m (Compound []) = Right m
evalStatement m (Compound (x:xs)) = case evalStatement m x of
    Left errs -> Left errs
    Right context -> evalStatement context (Compound xs)

-- tests

max' x y = If (x .< y) y x
expr1 = Var "x" .+ int 3
expr2 = If (Var "x") (Var "y" .- int 3) (int 2)
stat1 = Compound
    [ "x" @= int 3 .+ int 4
    , "y" @= Var "x" .* int 6
    , "z" @= neg $ max' (Var "x") (Var "y")
    ]
stat2 = Compound
    [ "r" @= int 1
    , "i" @= int 0
    , While (Var "i" .< Var "n") $ Compound
        [ "i" @= Var "i" .+ int 1
        , "r" @= Var "r" .* Var "i"
        ]
    ]

testsExpr = [ errorsCount (evalExpr M.empty expr1) ~?= 1
            , evalExpr (M.fromList [("x", B True), ("y", I 5)]) expr2 ~?= Right (I 2)
            , evalExpr (M.fromList [("x", B False), ("y", B False)]) expr2 ~?= Right (I 2)
            , errorsCount (evalExpr (M.fromList [("x", B True), ("y", B False)]) expr2) ~?= 1
            , fmap (M.lookup "z") (evalStatement M.empty stat1) ~?= Right (Just $ I $ -42)
            , fmap (M.lookup "r") (evalStatement (M.fromList [("n", I 6)]) stat2) ~?= Right (Just $ I 720)
            ]
  where errorsCount = either length (const 0)

------------------------------------------------------------------------------
-- 4. Реализовать двоичное дерево поиска без балансировки.

data Map k v = Leaf | Branch k v (Map k v) (Map k v)

lookup :: Ord k => k -> Map k v -> Maybe v
lookup _ Leaf = Nothing
lookup x (Branch k v left right) | x < k = lookup x left
                                 | x == k = Just v
                                 | otherwise = lookup x right

insert :: Ord k => k -> v -> Map k v -> (Map k v, Maybe v)
insert k v Leaf = (Branch k v Leaf Leaf, Just v)
insert x y (Branch k v left right) | x < k = let result = insert x y left in (Branch k v (fst result) right, snd result)
                                   | x == k = (Branch k v left right, Nothing)
                                   | otherwise = let result = insert x y right in (Branch k v left (fst result), snd result)

delete :: Ord k => k -> Map k v -> Maybe (Map k v)
delete _ Leaf = Nothing
delete x (Branch k v left right) | x < k = Just $ Branch k v (fromMaybe (delete x left)) right
                                 | x == k = Just $ deleteRoot (Branch k v left right)
                                 | otherwise = Just $ Branch k v left $ fromMaybe $ delete x right

fromMaybe :: Maybe (Map k v) -> (Map k v)
fromMaybe Nothing = Leaf
fromMaybe (Just m) = m

deleteRoot :: Ord k => Map k v -> Map k v
deleteRoot (Branch _ _ Leaf right) = right
deleteRoot (Branch _ _ left Leaf) = left
deleteRoot (Branch _ _ left right) = Branch (fst result) (snd result) left right where result = leftist right

leftist :: Ord k => Map k v -> (k, v)
leftist (Branch k v Leaf right) = (k, v)
leftist (Branch _ _ left _) = leftist left

fromList :: Ord k => [(k, v)] -> Map k v
fromList [] = Leaf
fromList ((k, v):xs) = fst $ insert k v $ fromList xs

toList :: Map k v -> [(k, v)]
toList Leaf = []
toList (Branch k v left right) = toList left ++ [(k, v)] ++ toList right

-- tests

sort :: Ord a => [a] -> [a]
sort = map fst . toList . fromList . map (\x -> (x, ()))

------------------------------------------------------------------------------
-- main

main = fmap (\_ -> ()) $ runTestTT $ test
      $  label "complex" testsComplex
      ++ label "height" testsHeight
      ++ label "avg" testsAvg
      ++ label "width" testsWidth
      ++ label "Expr" testsExpr
      ++ label "Map" -- можете сами написать тесты на каждую функцию :)
            [ sort [10,24,13,56,35,13,6,23] ~?= [6,10,13,23,24,35,56] ]
  where
    label :: String -> [Test] -> [Test]
    label l = map (\(i,t) -> TestLabel (l ++ " [" ++ show i ++ "]") t) . zip [1..]
