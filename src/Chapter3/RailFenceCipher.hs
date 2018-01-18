module Chapter3.RailFenceCipher where


{-}
data Rails = MkRails 
    {
        inputString :: String,
        rail1 :: String,
        rail2 :: String,
        rail3 :: String,
        currentRail :: Int,
        movementDirection :: Int
    }deriving (Eq,Show,Ord)
-}

------Positional product types


data Rails = MkRails String deriving (Eq, Show,Ord)

withoutSpaces :: String -> String
withoutSpaces xs = (filter (/=' ') xs)
 





encodeMessage :: String -> String -> String -> String -> Int -> Int -> Rails 
encodeMessage inputString rail1 rail2 rail3 currentRail movementDirection = if(inputString /= "") 
                                                                                then  
                                                                                    if(currentRail == 1)
                                                                                        then encodeMessage (tail inputString) (rail1++[(head inputString)]) rail2 rail3 (currentRail-movementDirection) (movementDirection)
                                                                                    else if(currentRail ==2)
                                                                                        then encodeMessage (tail inputString) rail1 (rail2++[(head inputString)]) rail3 (currentRail-(movementDirection)) (-movementDirection)
                                                                                    else encodeMessage (tail inputString) rail1 rail2 (rail3++[(head inputString)]) (currentRail-1) (movementDirection)
                                                                            else MkRails( rail1++rail2++rail3)
                                                                           
      


main = do
        print ( encodeMessage (withoutSpaces "WE ARE DISCOVERED FLEE AT ONCE") [] [] [] 1 (-1))           --WECRLTEERDSOEEFEAOCAIVDEN
        print ( encodeMessage (withoutSpaces "helloworld") [] [] [] 1 (-1))                               --holelwrdlo
        print ( encodeMessage (withoutSpaces "hello") [] [] [] 1 (-1))                                    --hoell
        print ( encodeMessage (withoutSpaces "1234567890") [] [] [] 1 (-1))                               --1592468037







{-}
encodeMessage :: Rails -> String 
encodeMessage MkRails{inputString =ips, rail1=r1,rail2=r2,rail3=r3,currentRail=cr,movementDirection =md} = 
                                                                            if(ips /= "") 
                                                                                then  
                                                                                    if(cr == 1)
                                                                                        then encodeMessage (tail ips) (rail1++[(head ips)]) rail2 rail3 (cr-md) (md)
                                                                                    else if(cr ==2)
                                                                                        then encodeMessage (tail ips) r1 (r2++[(head ips)]) rail3 (cr-(md)) (-md)
                                                                                    else encodeMessage (tail ips) r1 r2 (r3++[(head ips)]) (cr-1) (md)
                                                                            else r1++r2++r3
                                                                           
-} 
   {-}                                                                         
main = do
        print ( encodeMessage (withoutSpaces "WE ARE DISCOVERED FLEE AT ONCE") [] [] [] 1 (-1))           --WECRLTEERDSOEEFEAOCAIVDEN
        print ( encodeMessage (withoutSpaces "helloworld") [] [] [] 1 (-1))                               --holelwrdlo
        print ( encodeMessage (withoutSpaces "hello") [] [] [] 1 (-1))                                    --hoell
        print ( encodeMessage (withoutSpaces "1234567890") [] [] [] 1 (-1))    
        -}                           --1592468037