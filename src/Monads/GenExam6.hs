module Chapter2.GenExam2 where
import Data.List
import Data.Char
import Text.Read
import Data.List.Split

{-
score=[ ("Mojo", [("English", 84), ("Chemistry", 81), ("Physics", 95), ("Geography", 75)])
    , ("John Doe", [("Chemistry", 80), ("Physics", 95), ("Geography", 75)])
    , ("Captain Jack", [("Chemistry", 66), ("Phsyics", 33), ("Geography", 56)]) -- Note the spelling error in "Physics"
    , ("John Doe", [("Chemistry", 90), ("Economics", 45), ("Geography", 56)]) -- Note that "John Doe" is a repeat entry
    , ("Bahubali", [("Hindi", 45), ("Biology", -90), ("Geography", -75)]) -- Note the negative marks in "Biology" & "Geography"
    , ("Dexter", [("Tamil", 110), ("Biology", 100), ("Geography", 100), ("Physics", -55)]) -- Note that marks in "Tamil" are greater than 100
    ]
-}
   


validsubject :: [String]
validsubject = ["English", "Geography", "Physics", "Chemistry", "Economics", "Computer Science"]

type InputScore = [(String,[(String,Int)])]
type Subject = String
type SubjectAverage = (String,Float)     

type ValidSubjects = [String]
type StudentName = String
type Marks = Int
type Validate = (Bool,String)         


validateMarks :: InputScore -> [(String, [(String, Int, Bool, String)])]
validateMarks score =
    let validmarks = map (\(studentName,scorenames)->(studentName,             
                        concatMap(\(subjectname,marks) ->
                            if (marks < 0)
                                then [(subjectname,marks,False,"negative marks")]
                            else if (marks > 100)
                                then [(subjectname,marks,False,"marks higher than 100")]
                            else if(subjectname `elem` validsubject)
                                then [(subjectname,marks,True,"valid Subject")]
                            else [(subjectname,marks,False,"unknown Subject")]  ) scorenames )) score
        in validmarks



validateMarksSubjects ::(b->  (StudentName,Bool,[(Subject,Marks,Bool,String)]) -> b ) ->b -> InputScore -> ValidSubjects -> b
validateMarksSubjects testfunc accumulator score vsubjects= 
    let dupicatenames = (duplicateStudents score vsubjects)
        --newscore = filter (\(x,y)-> x `notElem` dupicatenames) score
        validMarks = validateMarks score
        newscore =  map(\(studentname,subjectinfo) -> 
                            if (studentname `elem` dupicatenames) 
                                then (studentname,False,subjectinfo) 
                            else (studentname,True,subjectinfo)) validMarks
    in foldl' (\x y -> testfunc x y) accumulator newscore 



type DuplicateStudentList =  [String] 
                                                                                        
duplicateStudents :: InputScore -> ValidSubjects -> DuplicateStudentList
duplicateStudents score vsubjects = 
    let studentnames = map (\ x  ->  fst x ) score
        duplicatenames = concatMap(\x -> if(length x > 1) then [head x] else [] ) (group(sort studentnames) )
                        --nub $ concat(filter(\x -> (length(x)>1 ))  (groupBy(\x y-> x==y) (sort(fst (unzip(score)))))) 
        in duplicatenames


invalidScores :: InputScore -> ValidSubjects  -> [(StudentName,[(Subject,Marks,String)])]
invalidScores score vsubjects =  
        let invalidData3 = validateMarksSubjects (\newlist (studentname,validstudent,subjectinfo) ->                         
                                newlist++[(studentname, (concatMap (\(subjectname,marks,validsubject,validmessage)->
                                            if(validsubject==False)
                                                then [(subjectname,marks,validmessage)]
                                            else [] )) subjectinfo)] ) [] score vsubjects
            in invalidData3


averageMarks :: InputScore -> Subject -> ValidSubjects -> Float
averageMarks score subjectName vsubject =
    let markslist = validateMarksSubjects (\newlist (studentname,validstudent,subjectinfo) ->
                                newlist++(concatMap (\(subjectname,marks,validsubject,validmessage)->
                                    if(validstudent==True && subjectname==subjectName && validsubject == True)
                                        then [marks]
                                    else [] )) subjectinfo 
                                ) [] score vsubject
        averageSubject =  ( fromIntegral( sum( markslist) ) / fromIntegral(length markslist))                                          
     in averageSubject



standardDeviation :: InputScore -> Subject -> ValidSubjects -> Float
standardDeviation score subjectName vsubject = 
    let averagevalue = averageMarks score subjectName vsubject                              
        listsub = validateMarksSubjects (\newlist (studentname,validstudent,subjectinfo) ->
                        newlist++(concatMap (\(subjectname,marks,validsubject,validmessage)->
                            if(validstudent==True && subjectname==subjectName && validsubject == True)
                                then [marks]
                            else [] )) subjectinfo 
            ) [] score  vsubject
        calculateXM = ( zipWith (\x y -> x-y)  listsub [round(averagevalue)..] )
        calculateXMSquare = sum (map (^2) (calculateXM) )
    in sqrt(fromIntegral(calculateXMSquare `div` (length(listsub) ) )   )  


------------------

validSubjectsStudentList :: [(Subject, [StudentName])]
validSubjectsStudentList = [("English", []),("Geography", []),("Physics", []),("Chemistry", []),("Economics", []),("Computer Science", [])]



-------------------------------------------

studentsListForExam1 :: InputScore -> ValidSubjects -> [(Subject, [StudentName])]
studentsListForExam1 score vsubject= 
    let listsub =validateMarksSubjects (\newlist (studentname,validstudent,subjectinfo) ->
                        newlist++(concatMap (\(subjectname,marks,validsubject,validmessage)->
                            if(validstudent==True && validsubject == True)
                                then [(subjectname,studentname)]
                            else [] )) subjectinfo 
            ) [] score vsubject
                                                                                                        --[("English","Mojo"),("Chemistry","Mojo"),("Physics","Mojo"),("Geography","Mojo"),("Chemistry","Captain Jack"),("Geography","Captain Jack"),("Geography","Dexter")]                   
        studentlist = map (\z -> foldl' (\ (subjectname,studentname) (x,y) ->
                                            if(x == subjectname)
                                                then (subjectname,studentname++[y])
                                            else (subjectname,studentname)   )
                                        z listsub ) validSubjectsStudentList 
    in studentlist
                                                                                                            --[("English",["Mojo"]),("Geography",["Mojo","Captain Jack","Dexter"]),("Physics",["Mojo"]),("Chemistry",["Mojo","Captain Jack"]),("Economics",[]),("Computer Science",[])]




grouping :: InputScore -> ValidSubjects -> [([Subject], [StudentName])]
grouping score vsubjects = 
    let listofstudent = validateMarksSubjects (\newlist (studentname,validstudent,subjectinfo) ->
                        if(validstudent==True)
                            then newlist++[(studentname,concatMap(\(subjectname,marks,validsubject,validmessage) -> 
                                    if validsubject
                                        then [subjectname]
                                    else []
                                    ) subjectinfo)]
                        else newlist
                        ) [] score vsubjects

                                                                                                                        --[("Mojo",["English","Chemistry","Physics","Geography"]),("Captain Jack",["Chemistry","Geography"]),("Bahubali",[]),("Dexter",["Geography"])]                                            --1--[("Mojo",["English","Chemistry","Physics","Geography"]),("Captain Jack",["Chemistry","Geography"]),("Bahubali",[]),("Dexter",["Geography"])]  
        subjectStudentList = studentsListForExam1 score vsubjects 
                                                                                                                            --[("English",["Mojo"]),("Geography",["Mojo","Captain Jack","Dexter"]),("Physics",["Mojo"]),("Chemistry",["Mojo","Captain Jack"]),("Economics",[]),("Computer Science",[])]       
        subjectListbynames = delete [] (nub(map (\(y,z)-> z) listofstudent) )                           
        namesListBysub =  (nub(map(\(y,z)-> z) subjectStudentList))

        in zip (subjectListbynames) namesListBysub 







inputFile :: IO ()
inputFile = do
    putStrLn "Enter the score file"
    scorefile <- getLine 
    myscorefile <-  (readFile scorefile)
    putStrLn "Enter the subject filename"
    subjectfile <- getLine
    mysubjectfile <- (readFile subjectfile)
    main1 myscorefile mysubjectfile 





main1 :: String -> String -> IO()
main1 myscorefile mysubjectfile = do 
    putStrLn "Enter \n 1. Average \n 2. Duplicate \n 3. Std Deviation \n 4. Invalid scores \n 5. Students for exam \n 6. Exit"
    choice <- getLine 
    let fscore = scoreRecord1 myscorefile
        fsubject = formatSubjects mysubjectfile
    case fscore of
        Right fscore1 -> 
                        case ((readMaybe choice)::Maybe Int) of
                                Just 1 -> forAverage fscore1 fsubject >> main1 myscorefile mysubjectfile
                                Just 2 -> forDuplicates fscore1 fsubject >> main1 myscorefile mysubjectfile
                                Just 3 -> forStdDeviation fscore1 fsubject >> main1 myscorefile mysubjectfile
                                Just 4 -> forInvalidScores fscore1 fsubject >> main1 myscorefile mysubjectfile
                                Just 5 -> forStudentsListForExam1 fscore1 fsubject >> main1 myscorefile mysubjectfile
                                Just 6 -> putStrLn "Exit"
                                _ -> main1 myscorefile mysubjectfile
        Left err -> do
                    putStrLn err
                    inputFile



pad :: Int -> String -> String
pad num xs = " " ++ xs ++(replicate (num-(length xs) ) ' ')

forDuplicates :: Score -> ValidSubjects -> IO()
forDuplicates score vsubject = putStrLn (foldl' (\arr name -> arr ++ name ++ "\n") "" (duplicateStudents score vsubject))

forAverage :: Score -> ValidSubjects -> IO()
forAverage score vsubject=
    putStrLn "Enter the subject name" >> getLine >>= \subjectname -> if(subjectname `elem` vsubject )
                                                                        then print (averageMarks score subjectname vsubject)
                                                                     else putStrLn "Invalid SubjectName" >> forAverage score vsubject

forStdDeviation :: Score -> ValidSubjects -> IO()
forStdDeviation score vsubject = 
    putStrLn "Enter the subject name" >> getLine >>= \ subjectname -> if(subjectname `elem` vsubject )
                                                                        then print (standardDeviation score subjectname vsubject)
                                                                      else putStrLn "Invalid SubjectName" >> forStdDeviation score vsubject


forInvalidScores :: Score -> ValidSubjects -> IO()
forInvalidScores score vsubject = putStrLn (foldl' (\arr1 (student,validation)-> arr1 ++ 
    foldl' (\str (subject,marks,reason)-> str++pad 20 student++" | "++pad 20 subject++" | "++pad 20 (show marks)++" | "++pad 20  reason ++" | "++"\n")
                    [] validation ) [] (invalidScores score vsubject) )



forStudentsListForExam1 :: Score -> ValidSubjects -> IO()
forStudentsListForExam1 score vsubject = 
    putStrLn (foldl' (\arr1 (subjectname,list)-> arr1 ++"\n"++pad 20 subjectname++" | " ++ 
            foldl' (\arr2 (student)-> arr2++pad 20 student++" ") [] list )
                     [] (studentsListForExam1 score vsubject))




type Score = [(String,[(String,Int)])] 

formatSubjects :: String -> [String]
formatSubjects subjectString = 
            splitOn "," subjectString






            

scoreRecord1 :: String -> Either String Score
scoreRecord1 inString = 
    let finalop = formatScore inString
        in case finalop of 
                Left err -> Left err
                Right score1 ->Right score1




formatScore:: String -> Either String [(String, [(String, Int)])]
formatScore scoreString =
    let splitNewLine = splitOn "\n" scoreString
        splitList =  map (\x -> splitOn "," x) splitNewLine
        grouping1 = groupBy  (\a  x ->  (head a) ==  (head x ) ) splitList 

        in mapM (\ records -> function1 records ("",[]) ) grouping1




function1:: [[String]] -> (String, [(String, Int)]) -> Either String (String, [(String, Int)])
function1 records (name,score1) =
    if(records == [])
        then Right (name,score1)
    else case (head records) of
            ([]:_:_:[]) -> Left "Invalid name"
            (_:[]:_:[]) -> Left "Invalid subject"
            (_:_:[]:[]) -> Left "Invalid marks"
            (sname:ssubject:smarks:[]) -> case ((readMaybe smarks) :: Maybe Int) of
                                                Just mark1 -> if name==sname
                                                                then function1 (tail records) (sname,score1++[(ssubject,mark1)])
                                                               else function1 (tail records) (name,score1++[(ssubject,mark1)])
                                                Nothing -> Left "invalid marks"
            _ -> Left "Invalid"


--mapM (\x -> if (x > 4) then Nothing else Just x) [1..10]
