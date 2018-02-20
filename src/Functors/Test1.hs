module Functors.Test1 where

incIfRight :: Num a => Either e a -> Either e a
incIfRight xs = 
    case xs of
        (Right n) -> Right $ n+1
        (Left e)  -> Left e



showIfRight :: Show a => Either e a -> Either e String
showIfRight xs =
    case xs of
        (Right s) -> Right $ show s
        (Left e)  -> Left e
---------------------------------------

incEither :: Num a => Either e a -> Either e a
incEither m = fmap (+1) m


showEither :: Show a => Either e a -> Either e String
showEither s = fmap show s

---------------------------------

incEither' :: Num a => Either e a -> Either e a
incEither' = fmap (+1)


showEither' :: Show a => Either e a -> Either e String
showEither' = fmap show

-------------------------

liftedInc :: (Functor f, Num b) => f b -> f b
liftedInc = fmap (+1)


liftedShow :: (Functor f, Show a) => f a -> f String
liftedShow = fmap show








data Sum a b = First a | Second b

instance Functor (Sum e) where
    fmap f (First a) = First a
    fmap f (Second b) = Second $ f b

