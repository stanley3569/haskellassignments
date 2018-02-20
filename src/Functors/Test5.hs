module Functors.Test5 where

data LinkedList a = LLEmpty | LLNode a (LinkedList a) deriving (Eq, Show)

instance Foldable LinkedList where
    foldMap fn ll =
        case ll of
            LLEmpty -> iv                  -- – TODO – is this correct? 
            LLNode x ll2 -> let newIv = fn iv x in Prelude.foldl fn newIv ll2
    null ll = Prelude.null $ foldMap (x -> [x]) ll

    