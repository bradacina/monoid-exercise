module JoinList where

data JoinList m a = 
    Empty
    | Single m a
    | Append m (JoinList m a) (JoinList m a)
    deriving (Eq, Show)

(+++) :: Monoid m => JoinList m a -> JoinList m a -> JoinList m a
(+++) x Empty = x
(+++) Empty x = x
(+++) x y =
    let 
        concatTag = mappend (tag x) (tag y)
    in
        Append (concatTag) x y

tag :: Monoid m => JoinList m a -> m
tag Empty = mempty
tag (Single m _) = m
tag (Append m _ _) = m