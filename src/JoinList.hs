module JoinList where

import Sized

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

indexJ :: (Sized b, Monoid b) => Int -> JoinList b a -> Maybe a
indexJ _ Empty = Nothing
indexJ n _ | n < 0 = Nothing
indexJ 0 (Single _ a) = Just a
indexJ _ (Single _ _) = Nothing
indexJ 0 (Append _ _ _) = Nothing
indexJ n (Append m left right) 
    | n >= (getSize . size $ m) = Nothing
    | n < leftSize = indexJ n left
    | n >= leftSize = indexJ (n - leftSize) right
    where leftSize = getSize . size . tag $ left