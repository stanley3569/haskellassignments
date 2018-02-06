module Chapter3.Stack1 where

import Data.List 

data Stack = EmptyStack  | StackNode Int Stack deriving (Eq, Show, Ord)                  



push :: Int -> Stack -> Stack               --1
push x stackl = StackNode x stackl


pop :: Stack -> Stack
pop stackl =
    case stackl of
        EmptyStack -> EmptyStack
        StackNode a b -> b    


