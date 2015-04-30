{-# LANGUAGE FlexibleContexts #-}

import Control.Monad.State
import Control.Monad.Writer
import Control.Monad.Reader
import Control.Monad.Error
import qualified Data.Map as M

--runWriter $ runErrorT $ runReaderT (runStateT (makeEquations (Lam "x" (Var "x") :@ (Var "y"))) 0) $ M.fromList [("y", TVar "y")]

type Symb = String
infixl 2 :@
data Expr = Var Symb | Expr :@ Expr | Lam Symb Expr deriving (Eq, Show)

infixr 3 :->
data Type = TVar Symb | Type :-> Type deriving (Eq, Show)

makeEquations :: (MonadReader VarMap m, MonadWriter [Equation] m, MonadError String m, MonadState Integer m) => Expr -> m Type
makeEquations (Var s) = do
	vs <- getVarTypes
	case M.lookup s vs of
		Just t -> return t
		Nothing -> throwError $ "variable \'" ++ s ++ "\' is not defined"
makeEquations (Lam s e) = freshType >>= \ t -> updateVarTypes s t (makeEquations e)
makeEquations (e :@ e') = do
	te <- makeEquations e
	te' <- makeEquations e'
	fresh <- freshType
	addEquation $ te := (te' :-> fresh)
	return fresh

type VarMap = M.Map Symb Type

getVarTypes :: MonadReader VarMap m => m VarMap
getVarTypes = ask

updateVarTypes :: MonadReader VarMap m => Symb -> Type -> m a -> m a
updateVarTypes s t c = local (M.insert s t) c

freshType :: MonadState Integer m => m Type
freshType = do
	i <- get
	put $ i + 1
	return $ TVar $ show i

infixr 4 :=
data Equation = Type := Type deriving (Eq, Show)

addEquation :: MonadWriter [Equation] m => Equation -> m ()
addEquation t = tell [t]