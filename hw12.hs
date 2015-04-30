-- Реализуйте алгоритм вывода типов.
-- (10 баллов)

data Term = Var String | App Term Term | Lam String Term
data Type = TVar String | Arr Type Type

inferType :: Term -> Either String Type
inferType = undefined
