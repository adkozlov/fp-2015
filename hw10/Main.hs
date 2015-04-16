import Test.HUnit
import Data.IORef

-- 1. Функция tagTree должна нумеровать узлы в дереве следующим образом:
-- В пронумерованном дереве должны встречаться все числа от 0 до (n - 1) ровно по одному разу, где n - число узлов в дереве.
-- Все номера в левом поддереве любого узла должны быть меньше номера этого узла, а в правом поддереве больше.
-- При использовании State (2 балла), без использования State (-100 баллов).
data Tree a = Node a (Tree a) (Tree a) | Leaf deriving (Eq,Show)

tagTree :: Tree a -> Tree (a, Int)
tagTree = undefined

tree1  = Node "a"     (Node "q"     (Node "t"     Leaf Leaf) (Node "r"     Leaf Leaf)) (Node "x"     Leaf Leaf)
tree1r = Node ("a",3) (Node ("q",1) (Node ("t",0) Leaf Leaf) (Node ("r",2) Leaf Leaf)) (Node ("x",4) Leaf Leaf)
tree2  = Node "a"     (Node "q"     Leaf Leaf) (Node "x"     (Node "s"     Leaf Leaf) (Node "f"     Leaf Leaf))
tree2r = Node ("a",1) (Node ("q",0) Leaf Leaf) (Node ("x",3) (Node ("s",2) Leaf Leaf) (Node ("f",4) Leaf Leaf))

-- 2. Напишите while.
-- (1 балл)
while :: Monad m => m Bool -> m a -> m [a]
while = undefined

fac :: Int -> IO [Int]
fac n = do
    i <- newIORef 0
    r <- newIORef 1
    return ()
    while (readIORef i >>= \i' -> return $ i' < n) $ do
        r' <- readIORef r
        i' <- readIORef i
        writeIORef i (i' + 1)
        writeIORef r ((i' + 1) * r')
        return r'

----------------------------------------------------------

main = fmap (const ()) $ runTestTT $ test
    $    label "tagTree"
    [ tagTree tree1 ~?= tree1r
    , tagTree tree2 ~?= tree2r
    ] ++ label "while"
    [ TestCase $ do
        r <- fac 6
        r @?= [1,1,2,6,24,120]
    ]
  where
    label :: String -> [Test] -> [Test]
    label l = map (\(i,t) -> TestLabel (l ++ " [" ++ show i ++ "]") t) . zip [1..]
    
    isLeft (Left _) = True
    isLeft (Right _) = False
