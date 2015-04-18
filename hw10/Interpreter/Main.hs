-- (2 балла)
{-# LANGUAGE TupleSections #-}

import Control.Monad
import qualified Data.Map as M
import Test.HUnit

import Expr
import Eval

getInt :: Eval Value -> Eval Integer
getInt m = m >>= handle where
    handle (I i) = return i
    handle _ = fail "type mismatch, expected type: Int"

getBool :: Eval Value -> Eval Bool
getBool m = m >>= handle where
    handle (B b) = return b
    handle _ = fail "type mismatch, expected type: Bool"

if' :: Eval Value -> Eval () -> Maybe (Eval ()) -> Eval ()
if' c t e = getBool c >>= if'' t e

if'' t (Just e) b = if b then t else e
if'' t Nothing b = when b t

getIntValue e = getInt $ evalExpr e
getBoolValue e = getBool $ evalExpr e

eval' l r c o = do
    l' <- getIntValue l
    r' <- getIntValue r
    return $ c (l' `o` r')

eval'' l r o = do
    l' <- getBoolValue l
    r' <- getBoolValue r
    return $ B (l' `o` r')

evalExpr :: Expr -> Eval Value
evalExpr (Const c) = return c
evalExpr (Var v) = getVar v
evalExpr (BinOp Plus l r) = eval' l r I (+)
evalExpr (BinOp Minus l r) = eval' l r I (-)
evalExpr (BinOp Mul l r) = eval' l r I (*)
evalExpr (BinOp And l r) = eval'' l r (&&)
evalExpr (BinOp Or l r) = eval'' l r (||)
evalExpr (BinOp Less l r) = eval' l r B (<)
evalExpr (BinOp Greater l r) = eval' l r B (>)
evalExpr (BinOp Equals l r) = eval' l r B (==)
evalExpr (UnOp Neg r) = liftM (I . negate) $ getIntValue r
evalExpr (UnOp Not r) = liftM (B . not) $ getBoolValue r

evalStatement :: Statement -> Eval ()
evalStatement (Compound []) = return ()
evalStatement (Compound (s:ss)) = evalStatement s >> evalStatement (Compound ss)
evalStatement (While e s) = if' (evalExpr e) (evalStatement s >> evalStatement (While e s)) Nothing
evalStatement (Assign s e) = do
    e' <- evalExpr e
    update s e'
evalStatement (If e s s') = if' (evalExpr e) (evalStatement s) (fmap evalStatement s')

------------------------------------------------------------------------------------------------
-- tests
------------------------------------------------------------------------------------------------

test1 = not_ (Var "x") .| Var "y" .< Const (I 3) .& Var "z" .= Var "y" .&
    Const (I 5) .< Var "y" .+ Const (I 7) .* Var "z" .+ Var "y" .* Const (I 3)

test2 = neg (Const $ I 5) .+ neg (Const $ I 3) .* Const (I 2) .- Const (I 7)

test3 = Compound
    [ "r" $= Const (I 1)
    , While (Var "n" .> Const (I 0)) $ Compound
        [ "r" $= Var "r" .* Var "n"
        , "n" $= Var "n" .- Const (I 1)
        ]
    ]

main = fmap (\_ -> ()) $ runTestTT $ test
    [ TestCase $ assertBool "Expected an error" $ errorsCount (runEval (evalExpr test1) $ M.fromList [("x",I 3),("y",I 5),("f",I 5)]) > 0
    , let m = M.fromList [("x",B True),("y",I 5),("z",I 5)] in runEval (evalExpr test1) m ~?= (Just (B False), [], m)
    , let m = M.fromList [("x",B True),("y",I 2),("z",I 2)] in runEval (evalExpr test1) m ~?= (Just (B True ), [], m)
    , runEval (evalExpr test2) M.empty ~?= (Just (I (-18)), [], M.empty)
    , runEval (evalStatement test3) (M.fromList [("n",I 5)]) ~?= (Just (), [], M.fromList [("n",I 0),("r",I 120)])
    ]
  where
    errorsCount (_,es,_) = length es
