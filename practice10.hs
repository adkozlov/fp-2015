{-#LANGUAGE TypeSynonymInstances, FlexibleInstances #-}

data Mu f = In (f (Mu f))

data BinF x = Empty | Zero x | One x

instance Functor BinF where
    fmap _ Empty = Empty
    fmap g (Zero x) = Zero (g x)
    fmap g (One x) = One (g x)

type Bin = Mu BinF

instance Show Bin where
    show = binToString

binFToInt :: BinF Int -> Int
binFToInt Empty = 0
binFToInt (Zero x) = 2 * x
binFToInt (One x) = 2 * x + 1

binFToString :: BinF String -> String
binFToString Empty = "0"
binFToString (Zero x) = x ++ "0"
binFToString (One x) = x ++ "1"

cata :: Functor f => (f a -> a) -> Mu f -> a
cata phi (In x) = phi $ fmap (cata phi) x

binToInt :: Bin -> Int
binToInt = cata binFToInt

binToString :: Bin -> String
binToString = cata binFToString

intToBinF :: Int -> BinF Int
intToBinF 0 = Empty
intToBinF x | even x = Zero xDiv2
            | otherwise = One xDiv2
            where xDiv2 = x `div` 2

ana :: Functor f => (a -> f a) -> a -> Mu f
ana psi x = In $ fmap (ana psi) (psi x)

intToBin :: Int -> Bin
intToBin = ana intToBinF

data ExprF x = Num Int | Plus x x | Mul x x

type Expr = Mu ExprF

instance Functor ExprF where
    fmap _ (Num x) = Num x
    fmap g (Plus x y) = Plus (g x) (g y)
    fmap g (Mul x y) = Mul (g x) (g y)

evalF :: ExprF Int -> Int
evalF (Num x) = x
evalF (Plus x y) = x + y
evalF (Mul x y) = x * y

eval :: Expr -> Int
eval = cata evalF