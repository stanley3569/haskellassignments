module RunLengthEncoding where

encodeTest :: String -> String
encodeTest xs =  encoding1 1 xs

encoding1 :: Int -> String -> String
encoding1 acc inputString =
    let headInput = [head inputString]
        tailInput = tail inputString 
        func = 
            if (tailInput == "")
                then (encoding2 acc headInput)
            else if (headInput == ([head tailInput]) )
                then encoding1 (acc+1) tailInput
            else (encoding2 acc headInput) ++ encoding1 1 tailInput
        in func

encoding2 :: Int -> String -> String
encoding2 acc x =
    let disp = 
            if(acc==1)
                then []
            else show acc
        join = disp ++ x
      in join

