module Functors.Test2 where



data LinkedList a = LLEmpty  | LLNode a (LinkedList a) deriving (Eq,Ord,Show)

x =(LLNode 30 (LLNode 50(LLNode 10 LLEmpty) ) )
y =(LLNode 65 (LLNode 75 (LLNode 40 (LLNode 80(LLNode 5 LLEmpty) ) ) ) )
z = LLEmpty


instance Monoid (LinkedList a) where
    mempty = LLEmpty
    mappend xs LLEmpty = xs
    mappend LLEmpty xs = mappend xs LLEmpty
    mappend (LLNode a b) ll2 = LLNode a $ mappend b ll2 
   

------------------------------------










----------------------
--functors


--data LinkedList a = LLEmpty  | LLNode a (LinkedList a) deriving (Eq,Ord,Show)


--x =(LLNode 30 (LLNode 20(LLNode 10 LLEmpty) ) )


instance Functor LinkedList where
    fmap _ LLEmpty = LLEmpty
    fmap f (LLNode a b) =LLNode (f a) (fmap f b)



--fmap (+22) x
-- ---> LLNode 52 (LLNode 42 (LLNode 32 LLEmpty))










