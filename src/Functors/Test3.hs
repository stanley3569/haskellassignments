module Functors.Test3 where
    
--import Data.Foldable
--import Data.Monoid

data Tree a = Leaf a | Branch (Tree a) (Tree a)

instance Foldable Tree where
 foldMap f (Leaf a)     = f a
 foldMap f (Branch l r) = foldMap f l `mappend` foldMap f r 




 --λ> let t = Branch (Branch (Leaf $ Sum 2) (Leaf $ Sum 4)) (Leaf $ Sum 6)
 --λ> fold t
 --Sum {getSum = 12}
