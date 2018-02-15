module NewConcepts.LinkList1 where

data LinkedList a = LLEmpty  | LLNode a (LinkedList a) deriving (Eq, Show, Ord)                 

--let a = LLEmpty
--let b = LLNode 100 a
--let c = LLNode 101 b
--let d = LLNode 102 c
--let e = LLNode 103 d
--test1 :: LinkedList a
--test1 =LLNode 103 (LLNode 102 (LLNode 101 (LLNode 100 LLEmpty))) 


linkDataList :: LinkedList a-> [a]                         
linkDataList listL =
    case listL of
        LLEmpty -> []
        LLNode a b -> [a]++(linkDataList b)

--Inserting an integer at the end 
linkListInsert :: a -> LinkedList a-> LinkedList a              
linkListInsert x listL = LLNode x listL


--Inserting an integer at the front 
linkListInsertStart :: a -> LinkedList a-> LinkedList a                  
linkListInsertStart x listL =
    case listL of
        (LLNode a LLEmpty) -> LLNode a (LLNode x LLEmpty)
        xs -> linkListInsert (lastL xs) (linkListInsertStart x (tailL xs))

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

 
                                                                            
lastL :: LinkedList a-> a                             
lastL listL =
    case listL of
        LLNode a _ -> a
        --LLEmpty -> 0

tailL :: LinkedList a-> LinkedList a                  
tailL listL =
    case listL of
        LLEmpty -> LLEmpty
        LLNode _ b -> b

--headL :: LinkedList -> Int                   --100
--headL listL =
--    case listL of 
--        LLNode a LLEmpty -> a
--        LLNode a b -> (headL (tailL b) )


linkListDelete :: LinkedList a-> LinkedList a 
linkListDelete listL = tailL listL


--Obtaining the length of a linked list
lengthL :: LinkedList a-> Int                    
lengthL listL =
    case listL of
        LLEmpty -> 0
        LLNode _ _ -> 1 + (lengthL (tailL listL))


{-}
indexL :: Int -> LinkedList a-> Int              --given position returns value
indexL x listL =
    case listL of
        LLNode a b  ->  if(x==0)
                            then a
                        else indexL (x-1) (tailL  (LLNode a b))         
-}

-- Returning the index of an element in the linked list

indexL1 ::Eq a => a -> LinkedList a-> Int             --7         --given value returns position
indexL1 x listL =
    case listL of
        LLNode a _ -> if(x==a)
                        then lengthL (tailL listL)
                      else indexL1 x (tailL listL)
        LLEmpty -> 0

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





--Breaking a linked list into two separate linked lists at the n-th position
breakL :: LinkedList a-> Int -> (LinkedList a,LinkedList a)
breakL listL pos = breakL1 listL (lengthL listL - pos - 1) listL 0

breakL1 :: LinkedList a-> Int -> LinkedList a-> Int -> (LinkedList a,LinkedList a)
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
            if pos == count-1
                then LLEmpty
            else LLNode a (breakL2 b pos (count+1))
        LLEmpty -> LLEmpty








{-}
done1    Inserting an integer at the front of the linked-list
done2    Inserting an integer at the end of the linked-list
done3    Inserting an integer at the n-th position of a linked-list
done4    removing an integer at the n-th position of a linked-list
done5    Obtaining the length of a linked list
done6    Reversing a linked list
done7    Returning the index of an element in the linked list
done8    Breaking a linked list into two separate linked lists at the n-th position
-}   