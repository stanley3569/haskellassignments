module Functors.Test4 where

data LinkedList a = LLEmpty | LLNode a (LinkedList a) deriving (Eq, Show)

instance Foldable LinkedList where

    foldMap fn ll = case ll of
        LLEmpty -> mempty                                   -- – TODO – think about this? 
        LLNode x ll2 -> (fn x) `mappend` (foldMap fn ll2)

    --foldl :: (b -> a -> b) -> b -> LinkedList a -> b 
    --foldl fn iv ll = Prelude.foldl fn iv $ foldMap (x -> [x]) ll

