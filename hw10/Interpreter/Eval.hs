-- (2 балла)
module Eval
    ( Eval, runEval
    , Error, Store
    , update, getVar
    ) where

import qualified Data.Map as M
import Data.List
import Control.Monad
import Control.Applicative

import Expr

type Error = String
type Store = M.Map String Value

newtype Eval a = Eval { runEval :: Store -> (Maybe a, [Error], Store) }

instance Functor Eval where
    fmap f (Eval m) = Eval handle where
        handle s = case m s of
            (Just x, e, s') -> (Just $ f x, e, s')
            (Nothing, e, s') -> (Nothing, e, s')

instance Applicative Eval where
    pure x = Eval (\ s -> (Just x, [], s))
    Eval m <*> Eval k = Eval handle where
        handle s = case m s of
            (Just x, e, s') -> case k s of
                (Just y, e', s'') -> (Just $ x y, e ++ e', s'')
                (Nothing, e', s'') -> (Nothing, e ++ e', s'')
            (Nothing, e, s') -> (Nothing, e, s')

instance Monad Eval where
    return x = Eval (\ s -> (Just x, [], s))
    Eval m >>= k = Eval handle where
        handle s = case m s of
            (Just x, e, s') -> case runEval (k x) s' of
                (Just y, e', s'') -> (Just y, e ++ e', s'')
                (Nothing, e', s'') -> (Nothing, e ++ e', s'')
            (Nothing, e, s') -> (Nothing, e, s')
    fail e = Eval (\ s -> (Nothing, [e], s))

instance Alternative Eval where
    empty = Eval (\ s -> (Nothing, [], s))
    Eval l <|> Eval r = Eval handle where
        handle s = case l s of
            (Just x, e, s') -> (Just x, e, s')
            (Nothing, e, s') -> case r s of
                (Just y, e', s'') -> (Just y, e ++ e', s'')
                (Nothing, e', s'') -> (Nothing, e ++ e', s'')

-- MonadPlus - аналог Alternative для монад
-- mzero - вычисление, которое ничего не делает, сразу завершается неуспехом
-- mplus m1 m2 пытается выполнить m1, если тот завершился неуспехом, выполняет m2
-- Примеры использования этого класса есть в Utils.hs
instance MonadPlus Eval where
    mzero = empty
    mplus e e' = e <|> e'

update :: String -> Value -> Eval ()
update k v = Eval (\ s -> (Just (), [], M.insert k v s))

getVar :: String -> Eval Value
getVar k = Eval $ \ s -> case M.lookup k s of
    Just v -> (Just v, [], s)
    Nothing -> (Nothing, ["variable \'" ++ k ++ "\' not found"], s)