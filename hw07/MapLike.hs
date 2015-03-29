import Prelude hiding (lookup)
import qualified Data.Map as M
import qualified Data.List as L

-- (5 баллов)

-- 1. Определить класс MapLike типов, похожих на Map.
--    В нем должны быть функции empty, lookup, insert, delete, fromList с типами как в Data.Map.
--    Напишите реализацию по умолчанию для fromList.

class MapLike m where
    empty :: m k v
    lookup :: Ord k => k -> m k v -> Maybe v
    insert :: Ord k => k -> v -> m k v -> m k v
    delete :: Ord k => k -> m k v -> m k v
    fromList :: Ord k => [(k, v)] -> m k v
    fromList l = fromList' l empty where
    	         fromList' [] acc = acc
    	         fromList' ((k, v):xs) acc = fromList' xs $ insert k v acc

-- 2. Определить instance MapLike для Data.Map, ListMap и ArrMap
--    Можно использовать любые стандартные функции.

instance MapLike M.Map where
	empty = M.empty
	lookup = M.lookup
	insert = M.insert
	delete = M.delete
	fromList = M.fromList

newtype ListMap k v = ListMap [(k,v)]

instance MapLike ListMap where
    empty = ListMap []
    lookup k (ListMap l) = L.lookup k l
    insert k v (ListMap l) = ListMap (l ++ [(k, v)])
    delete k (ListMap l) = ListMap $ filter (\ (l, _) -> k /= l) l
    fromList = ListMap

newtype ArrMap k v = ArrMap (k -> Maybe v)

instance MapLike ArrMap where
    empty = ArrMap (\ _ -> Nothing)
    lookup k (ArrMap a) = a k
    insert k v (ArrMap a) = ArrMap (\l -> if k == l then Just v else a l)
    delete k (ArrMap a) = ArrMap (\l -> if k == l then Nothing else a l)

-- 3. Написать instace Functor для ListMap k и ArrMap k.

instance Functor (ListMap k) where
    fmap f (ListMap l) = ListMap $ map (\(k, v) -> (k, f v)) l

instance Functor (ArrMap k) where
    fmap f (ArrMap a) = ArrMap $ (\k -> maybe Nothing (Just . f) $ a k)