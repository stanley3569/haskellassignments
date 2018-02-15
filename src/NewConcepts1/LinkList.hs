module NewConcepts1.LinkList where


instance Eq a => Eq (LinkedList a) where
    (LLNode a _) == (LLNode c _) = (a == c)                                         
    LLEmpty == LLEmpty = True
    _ == _ = False

    
{-}
instance Ord a => Ord (LinkedList a) where
    compare (LLNode a _) (LLNode c _) = compare a c
    compare (LLEmpty) (LLEmpty) = EQ
-}

instance Ord a => Ord (LinkedList a) where
    (>=) (LLNode a _) (LLNode c _) = a >= c
    (>=) _ _ = False
    (<=) (LLNode a _) (LLNode c _) = a <= c
    (<=) _ _ = False
    (>) (LLNode a _) (LLNode c _) = a > c
    (>) _ _ = False
    (<) (LLNode a _) (LLNode c _) = a < c
    (<) _ _ = False


instance Show a => Show (LinkedList a) where
    show (LLNode a b) = show a ++ " " ++ show (b)
    show (LLEmpty) = ""

data LinkedList a = LLEmpty  | LLNode a (LinkedList a)            


    
linkDataList :: LinkedList a-> [a]                         
linkDataList listL =
        case listL of
            LLEmpty -> []
            LLNode a b -> reverse $ (linkDataList b)++ [a]
    
    --Inserting an integer at the end 
linkListInsert :: a -> LinkedList a-> LinkedList a              
linkListInsert x listL = LLNode x listL
    
    
--Inserting an integer at the front 
linkListInsertStart ::Eq a => a -> LinkedList a-> LinkedList a                  
linkListInsertStart x listL =
        case listL of
            LLEmpty -> LLNode x LLEmpty
            (LLNode a LLEmpty) -> LLNode a (LLNode x LLEmpty)
            xs -> linkListInsert (case xs of LLNode a _-> a) (linkListInsertStart x (tailL xs))
            

    -- Inserting an integer at the n-th position 
linkListInsertNPosition :: Int -> a -> LinkedList a-> LinkedList a                       
linkListInsertNPosition pos val listL =
        insertionN val (lengthL listL - pos) listL
        where insertionN val1 count listL1 =
                                            case listL1 of
                                                LLEmpty -> LLEmpty
                                                (LLNode a b) -> if (count <=0)
                                                                    then LLNode val1 (LLNode a b)
                                                                else LLNode a (insertionN val1 (count - 1) b)            


                                                                                
lastL1 :: LinkedList a-> Maybe a                             
lastL1 listL =
        case listL of
            LLNode a _ -> Just a
            LLEmpty -> Nothing
    
tailL :: LinkedList a-> LinkedList a                  
tailL listL =
        case listL of
            LLEmpty -> LLEmpty
            LLNode _ b -> b

linkListDelete :: LinkedList a-> LinkedList a 
linkListDelete listL = tailL listL
    
    
    --Obtaining the length of a linked list
lengthL :: LinkedList a-> Int                    
lengthL listL =
        case listL of
            LLEmpty -> 0
            LLNode _ _ -> 1 + (lengthL (tailL listL))
    

{-}  
indexL1 ::Eq a =>  a -> LinkedList a->  Int             --7         --given value returns position
indexL1 x listL =
        case listL of
            LLNode a _ -> if(x==a)
                            then lengthL (tailL listL)
                          else indexL1 x (tailL listL)
            LLEmpty -> 0
-}

indexL2 ::Eq a =>  a -> LinkedList a -> Maybe Int           --7         --given value returns position
indexL2 x listL =
        case listL of
            LLNode a _ -> if(x==a)
                            then Just (lengthL (tailL listL) )
                          else indexL2 x (tailL listL)
            LLEmpty -> Nothing


    --Reversing a linked list
reverseL :: LinkedList a-> LinkedList a-> LinkedList a
reverseL listL new = 
        case listL of 
            LLEmpty -> new
            LLNode a b -> reverseL b (LLNode a new)
    
    --removing an integer at the n-th position of a linked-list
deletionN :: LinkedList a-> Int -> LinkedList a                      
deletionN listL pos =
        removeNPosition listL pos (lengthL listL - pos - 2) 
            where 
                removeNPosition listL1 pos1 count = 
                    case listL1 of 
                        LLNode a b -> 
                            if (count < 0)
                                then
                                    let LLNode _ y = b
                                    in LLNode a y
                            else LLNode a (removeNPosition b pos1 (count-1))
                        LLEmpty -> LLEmpty
    

breakL :: LinkedList a-> Int -> (LinkedList a,LinkedList a)
breakL listL pos =
            breakL1 listL (lengthL listL - pos - 1) listL 0
            where 
                breakL1 listL1 poss pos1 count = 
                    case listL1 of 
                        LLNode _ b -> 
                            if (poss == count-1)
                                then (breakL2 pos1 poss 0,listL1)
                            else breakL1 b poss pos1 (count+1)
                        LLEmpty -> (LLEmpty,LLEmpty)

                breakL2 listL2 posss count = 
                    case listL2 of 
                        LLNode a b -> 
                            if (posss == count-1)
                                then LLEmpty
                            else LLNode a (breakL2 b posss (count+1))
                        LLEmpty -> LLEmpty

 {-} 
    --Breaking a linked list into two separate linked lists at the n-th position
breakL :: LinkedList a-> Int -> (LinkedList a,LinkedList a)
breakL listL pos = breakL1 listL (lengthL listL - pos - 1) listL 0
    
breakL1 ::  LinkedList a-> Int -> LinkedList a-> Int -> (LinkedList a,LinkedList a)
breakL1 listL pos pos1 count = 
        case listL of 
            LLNode _ b -> 
                if (pos == count-1)
                    then (breakL2 pos1 pos 0,listL)
                else breakL1 b pos pos1 (count+1)
            LLEmpty -> (LLEmpty,LLEmpty)
    
breakL2 :: LinkedList a-> Int -> Int -> LinkedList a
breakL2 listL pos count = 
        case listL of 
            LLNode a b -> 
                if (pos == count-1)
                    then LLEmpty
                else LLNode a (breakL2 b pos (count+1))
            LLEmpty -> LLEmpty
-}
