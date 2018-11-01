module Chapter3.LinkList where

import Data.List 

data LinkedList = LLEmpty  | LLNode Int LinkedList deriving (Eq, Show, Ord)                 

--let a = LLEmpty
--let b = LLNode 100 a
--let c = LLNode 101 b
--let d = LLNode 102 c
--let e = LLNode 103 d
a =LLNode 103 (LLNode 102 (LLNode 101 (LLNode 100 LLEmpty))) 


linkDataList :: LinkedList -> [Int]                         
linkDataList listL =
    case listL of
        LLEmpty -> []
        LLNode a b -> [a]++(linkDataList b)

--Inserting an integer at the end 
linkListInsert :: Int -> LinkedList -> LinkedList               
linkListInsert x listL = LLNode x listL


--Inserting an integer at the front 
linkListInsertStart :: Int -> LinkedList -> LinkedList                   
linkListInsertStart x listL =
    case listL of
        (LLNode a LLEmpty) -> LLNode a (LLNode x LLEmpty)
        xs -> linkListInsert (lastL xs) (linkListInsertStart x (tailL xs))

-- Inserting an integer at the n-th position 
linkListInsertNPosition :: Int -> Int -> LinkedList -> LinkedList                       
linkListInsertNPosition pos val listL =
    let ninsertion = insertionN val (lengthL listL - pos) listL
        in ninsertion

insertionN :: Int -> Int -> LinkedList -> LinkedList           
insertionN val count listL =
    case listL of
        LLEmpty -> LLEmpty
        (LLNode a b) -> if (count <=0)
                            then LLNode val (LLNode a b)
                        else LLNode a (insertionN val (count - 1) b)            

            
lastL :: LinkedList -> Int                             
lastL listL =
    case listL of
        LLNode a b -> a

tailL :: LinkedList -> LinkedList                   
tailL listL =
    case listL of
        LLEmpty -> LLEmpty
        LLNode a b -> b

--headL :: LinkedList -> Int                   --100
--headL listL =
--    case listL of 
--        LLNode a LLEmpty -> a
--        LLNode a b -> (headL (tailL b) )


linkListDelete :: LinkedList -> LinkedList  
linkListDelete listL = tailL listL


--Obtaining the length of a linked list
lengthL :: LinkedList -> Int                    
lengthL listL =
    case listL of
        LLEmpty -> 0
        LLNode a b -> 1 + (lengthL (tailL listL))


{-
indexL :: Int -> LinkedList -> Int              --given position returns value
indexL x listL =
    case listL of
        LLNode a b  ->  if(x==0)
                            then a
                        else indexL (x-1) (tailL  (LLNode a b))         
-}

-- Returning the index of an element in the linked list
indexL1 :: Int -> LinkedList -> Int             --7         --given value returns position
indexL1 x listL =
    case listL of
        LLNode a b -> if(x==a)
                        then lengthL (tailL listL)
                      else indexL1 x (tailL listL)
        

--Reversing a linked list
reverseL :: LinkedList -> LinkedList -> LinkedList
reverseL listL new = 
    case listL of 
        LLEmpty -> new
        LLNode a b -> reverseL b (LLNode a new)

--removing an integer at the n-th position of a linked-list
deletionN :: LinkedList -> Int -> LinkedList                       
deletionN listL pos =
    let ninsertion = removeNPosition listL pos (lengthL listL - pos - 2) 
        in ninsertion


removeNPosition ::  LinkedList-> Int -> Int -> LinkedList
removeNPosition listL pos count = 
    case listL of 
        LLNode a b -> 
                    if (count < 0)
                    then
                        let LLNode a y = b
                            in LLNode a y
                    else LLNode a (removeNPosition b pos (count-1))
        LLEmpty -> LLEmpty





--Breaking a linked list into two separate linked lists at the n-th position
breakL :: LinkedList -> Int -> (LinkedList,LinkedList)
breakL listL pos = breakL1 listL (lengthL listL - pos - 1) listL 0

breakL1 :: LinkedList -> Int -> LinkedList -> Int -> (LinkedList,LinkedList)
breakL1 listL pos pos1 count = 
    case listL of 
        LLNode a b -> 
            if (pos == count-1)
                then (breakL2 pos1 pos 0,listL)
            else breakL1 b pos pos1 (count+1)
        LLEmpty -> (LLEmpty,LLEmpty)

breakL2 :: LinkedList -> Int -> Int -> LinkedList
breakL2 listL pos count = 
    case listL of 
        LLNode a b -> 
            if pos == count-1
                then LLEmpty
            else LLNode a (breakL2 b pos (count+1))
        LLEmpty -> LLEmpty









{-
linkListInsertNPosition :: Int -> Int -> LinkedList -> LinkedList                       
linkListInsertNPosition pos val listL =
    let ninsertion = insertionN val (lengthL listL - pos) listL
        in ninsertion

insertionN :: Int -> Int -> LinkedList -> LinkedList           
insertionN val count listL =
    case listL of
        LLEmpty -> LLEmpty
        (LLNode a b) -> if (count <=0)
                            then LLNode val (LLNode a b)
                        else LLNode a (insertionN val (count - 1) b)     
-}

--let a =LLNode 103 (LLNode 102 (LLNode 101 (LLNode 100 LLEmpty)))
--LLNode 100 (LLNode 101 (LLNode 102 (LLNode 103 LLEmpty)))


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