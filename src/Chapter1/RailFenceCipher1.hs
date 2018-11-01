module Chapter1.RailFenceCipher where
withoutSpaces :: String -> String
withoutSpaces xs = (filter (/=' ') xs)
   

encodeMessage :: String -> String -> String -> String -> Int -> Int -> String 
encodeMessage inputString rail1 rail2 rail3 currentRail movementDirection = if(inputString /= "") 
                                                                                then  
                                                                                    if(currentRail == 1)
                                                                                        then encodeMessage (tail inputString) (rail1++[(head inputString)]) rail2 rail3 (currentRail-movementDirection) (movementDirection)
                                                                                    else if(currentRail ==2)
                                                                                        then encodeMessage (tail inputString) rail1 (rail2++[(head inputString)]) rail3 (currentRail-(movementDirection)) (-movementDirection)
                                                                                    else encodeMessage (tail inputString) rail1 rail2 (rail3++[(head inputString)]) (currentRail-1) (movementDirection)
                                                                            else rail1++rail2++rail3
                                                                           
                                                                               
main = do
        print ( encodeMessage (withoutSpaces "WE ARE DISCOVERED FLEE AT ONCE") [] [] [] 1 (-1))           --WECRLTEERDSOEEFEAOCAIVDEN
        print ( encodeMessage (withoutSpaces "helloworld") [] [] [] 1 (-1))                               --holelwrdlo
        print ( encodeMessage (withoutSpaces "hello") [] [] [] 1 (-1))                                    --hoell
        print ( encodeMessage (withoutSpaces "1234567890") [] [] [] 1 (-1))                               --1592468037