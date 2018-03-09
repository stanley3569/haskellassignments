module Monads.RailFenceCipher where

import Text.Read
import Data.Ord 
import Data.List

main :: IO()
main =
    putStrLn "Give Input \n 1. Encode the string \n 2. Decode the string \n 3. Exit" >>
                    getLine >>= \ choice ->
                            case (readMaybe choice)::Maybe Int of
                                Just 1 -> putStrLn "Enter the string to be encoded">> getLine >>= \instring ->
                                                putStrLn "Enter the no of rails">> getLine >>= \rails ->
                                                    case (readMaybe rails)::Maybe Int of
                                                        Just x -> putStrLn (encode x instring) >> main
                                                        Nothing -> putStrLn "Invalid no of rails" >> main

                                Just 2 -> putStrLn "Enter the string to be decoded">> getLine >>= \instring ->
                                                putStrLn "Enter the no of rails">> getLine >>= \rails ->
                                                    case (readMaybe rails)::Maybe Int of
                                                        Just x -> putStrLn (decode x instring) >> main
                                                        Nothing -> putStrLn "Invalid no of rails" >> main  
                                                    
                                Just 3 -> putStrLn "Exit"
                                Just _ -> putStrLn "Invalid input" >> main
                                Nothing -> main


    
encode :: Int -> [a] -> [a]
encode n xs = map (\x -> fst x)  (sortBy (comparing (\(x,y) -> y))  (zip xs rails) )
                    where rails = cycle ([1..n] ++ [n-1,n-2..2])
                        


decode :: Int -> String -> String
decode n xs =  map (\x -> snd x) ordChar
                    where 
                        posChar = (zip (encode n [0..(length xs-1)]) xs)
                        ordChar = sortBy (comparing (\(x,y) -> x)) posChar 










