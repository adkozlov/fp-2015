import Test.HUnit
import qualified Data.Map as M

-- На MapLike тестов нет.
import Complex
import Tree

-- Complex

testCasesComplex =
    [ (Complex 3 7, "3.0 + 7.0 * i")
    , (Complex (-3) 7, "-3.0 + 7.0 * i")
    , (Complex 3 (-7), "3.0 - 7.0 * i")
    , (Complex (-3) (-7), "-3.0 - 7.0 * i")
    , (Complex 3 0, "3.0")
    , (Complex (-3) 0, "-3.0")
    , (Complex 0 7, "7.0 * i")
    , (Complex 0 (-7), "-7.0 * i")
    ]

testsComplex =
    [ i * i ~?= -1
    , 3 + i ~?= Complex 3 1
    , 3 * i ~?= Complex 0 3
    , (3 + 4 * i) * (4 - 3 * i) ~?= 24 + 7 * i
    , 3.2 + 1.4 * i ~?= Complex 3.2 1.4
    , 2 / (1 + i) ~?= 1 - i
    , abs (4 - 3 * i) ~?= 5
    ] ++ map (\(c,s) -> show c ~?= s) testCasesComplex
      ++ map (\(c,s) -> read s ~?= c) testCasesComplex

-- Tree

tree1 = Node "a" [Node "b" [Node "f" []], Node "c" [Node "d" []], Node "e" []]
tree2 = Node 1 [Node 2 [Node 4 [], Node 5 []], Node 3 []]
tree3 = Node "1" [Node "2" [Node "4" [], Node "5" []], Node "3" []]

testsTree =
    [ show tree1 ~?= "\"a\":{\"b\":{\"f\"},\"c\":{\"d\"},\"e\"}"
    , show tree2 ~?= "1:{2:{4,5},3}"
    , fmap show tree2 ~?= tree3
    , (read "1:{2:{4,5},3}" :: Tree Int) ~?= tree2
    , (reads "1:{2:{4,5},}" :: [(Tree Int,String)]) ~?= [(Node 1 [],":{2:{4,5},}")]
    , (reads ",1:{2:{4,5},}" :: [(Tree Int,String)]) ~?= []
    ]

-- main

main = fmap (\_ -> ()) $ runTestTT $ test $
       label "Complex" testsComplex
    ++ label "Tree" testsTree
  where
    label :: String -> [Test] -> [Test]
    label l = map (\(i,t) -> TestLabel (l ++ " [" ++ show i ++ "]") t) . zip [1..]
