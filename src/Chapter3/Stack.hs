module Chapter3.Stack where
    
import Data.List 
    
data Stack = Stack[Int] | StartStack deriving (Eq, Show, Ord)  
                                                                   --sum type
 

push :: Stack -> Int -> Stack
push listL x= 
    case listL of 
        Stack list -> Stack ([x]++list)
        StartStack -> Stack [x]



pop :: Stack -> Stack
pop xstack =
    case xstack of
        Stack x->Stack (tail x)

stackList :: Stack -> [Int]
stackList xstack =
    case xstack of
        Stack x -> x
