-- (3 балла)
module Eval
    ( EvalT, runEvalT
    , Eval, runEval
    , Store
    , update, getVar
    ) where

import qualified Data.Map as M
import Data.List
import Control.Monad
import Control.Applicative
import Control.Monad.Identity
import Control.Monad.Trans

import Expr

type Store = M.Map String Value

-- newtype Eval a = Eval { runEval :: Store -> (Maybe a, [String], Store) }

-- Реализуйте трансформер монад EvalT.
newtype EvalT m a = EvalT ? -- напините сами
type Eval = EvalT Identity

runEvalT :: EvalT m a -> Store -> m (Maybe a, [String], Store)
runEvalT = undefined

runEval :: Eval a -> Store -> (Maybe a, [String], Store)
runEval = undefined

instance Functor m => Functor (EvalT m) where
    fmap = undefined

instance (Functor m, Monad m) => Applicative (EvalT m) where
    pure x = undefined
    m <*> k = undefined

instance Monad m => Monad (EvalT m) where
    return x = undefined
    m >>= k = undefined

instance (Functor m, MonadPlus m) => Alternative (EvalT m) where
    empty = undefined
    l <|> r = undefined

instance MonadPlus m => MonadPlus (EvalT m) where
    mzero = undefined
    mplus = undefined

instance MonadTrans EvalT where
    lift = undefined

update :: String -> Value -> Eval ()
update = undefined

getVar :: String -> Eval Value
getVar = undefined
