module Chapter3.RailFenceCipher3 where
withoutSpaces :: String -> String
withoutSpaces xs = (filter (/=' ') xs)

type InputString = String
type Rail1 = String
type Rail2 = String
type Rail3 = String
type CurrentRail = Int
type MovementDirection = Int
type EncodedString = String

encodeMessage :: InputString -> Rail1 -> Rail2 -> Rail3 -> CurrentRail -> MovementDirection -> EncodedString 
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