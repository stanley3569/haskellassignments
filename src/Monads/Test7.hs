module Chapter2.GenExam2 where

import Data.List
import Text.Read
import Data.List.Split

formatScore:: [Char] -> Either String [(String, [(String, String)])]
formatScore scoreString =
    let splitNewLine = splitOn "\n" scoreString
        splitList =  map (\x -> splitOn "," x) splitNewLine
        grouping1 = groupBy  (\a  x ->  (head a) ==  (head x ) ) splitList 

        in mapM (\ records -> function1 records ("",[]) ) grouping1




function1:: [[String]] -> (String, [(String, String)]) -> Either String (String, [(String, String)])
function1 records (name,score1) =
    if(records == [])
        then Right (name,score1)
    else case (head records) of
            ([]:ssubject:smarks:[]) -> Left "Invalid name"
            (ssname:[]:smarks:[]) -> Left "Invalid subject"
            (ssname:ssubject:[]:[]) -> Left "Invalid marks"
            ([]:[]:[]:[]) -> Left "Invalid Result"
            (sname:ssubject:smarks:[]) -> case ((readMaybe smarks) :: Maybe Int) of
                                                Just mark1 -> if name==""
                                                                then function1 (tail records) (sname,score1++[(ssubject,smarks)])
                                                               else function1 (tail records) (name,score1++[(ssubject,smarks)])
                                                Nothing -> Left "invalid marks"


                                                                {-}
                                                                else function1 (tail records) ("valid",
                                                                        let (student,listMarksSubject) = last score1
                                                                            in if student==sname 
                                                                                    then (init score1)++[(sname,listMarksSubject++[(ssubject,smarks)])]
                                                                                else score1 ++ [(sname,listMarksSubject++[(ssubject,smarks)])]
                                                                    )
-}


{-}
function1 records data1 = 
                    (case records of 
                        (studentname,subject,marks) ->
                            case studentname of 
                                [] -> Left "Invalid name"
                                xname ->  (case subject of 
                                                    [] -> Left "invalid subject"
                                                    xsubject -> case ((readMaybe marks):: Maybe Int ) of 
                                                                    Nothing -> Left "invalid marks" 
                                                                    Just xmarks -> Right (studentname,subject,xmarks) )  )
-}



       -- grouping1 = groupBy (\ (x,y,z) (a,b,c)-> x==a) grouping
        --grouping2 = groupBy (\x y -> x == y) splitList
       -- grouping2 = groupBy (\(x:y:z:[]) (a:b:c:[]) -> x == a) splitList


       -- [["student1","subject1","marks1 "],[" student1","subject2","marks3 "],[" student2","subject2","marks2 "],["student3","subject3","marks3"]]

       --formatScore "student1,subject1,marks1 \nstudent1,subject2,marks3 \nstudent2,subject2,marks2 \nstudent3,subject3,marks3"
       --[ [["student1","subject1","marks1 "],["student1","subject2","marks3 "]],[["student2","subject2","marks2 "]],[["student3","subject3","marks3"]]]

