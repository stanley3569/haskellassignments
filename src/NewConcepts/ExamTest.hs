module NewConcepts.ExamTest where
import Data.List

-----------------------------------------------------------------

score :: [(String,[(String,Int)])]
score=[ ("Mojo", [("English", 84), ("Chemistry", 81), ("Physics", 95), ("Geography", 75)])
    , ("John Doe", [("Chemistry", 80), ("Physics", 95), ("Geography", 75)])
    , ("Captain Jack", [("Chemistry", 66), ("Phsyics", 33), ("Geography", 56)]) -- Note the spelling error in "Physics"
    , ("John Doe", [("Chemistry", 90), ("Economics", 45), ("Geography", 56)]) -- Note that "John Doe" is a repeat entry
    , ("Bahubali", [("Hindi", 45), ("Biology", -90), ("Geography", -75)]) -- Note the negative marks in "Biology" & "Geography"
    , ("Dexter", [("Tamil", 110), ("Biology", 100), ("Geography", 100), ("Physics", -55)]) -- Note that marks in "Tamil" are greater than 100
    ]



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



validateMarksSubjects ::(b->  (StudentName,Maybe [(Subject,Marks,Bool,String)]) -> b ) ->b -> InputScore -> b
validateMarksSubjects testfunc accumulator score= 
    foldl' (\x y -> testfunc x y) accumulator newscore 
      where dupicatenames = (duplicateStudents score)
        --newscore = filter (\(x,y)-> x `notElem` dupicatenames) score
            validMarks = validateMarks score
            newscore =  map(\(studentname,subjectinfo) -> 
                            if (studentname `elem` dupicatenames) 
                                then (studentname,Nothing) 
                            else (studentname,Just subjectinfo)) validMarks




type DuplicateStudentList =  [String]                                                                                         
duplicateStudents :: InputScore -> DuplicateStudentList
duplicateStudents score = 
    concatMap(\x -> if(length x > 1) then [head x] else [] ) (group(sort studentnames) )
                        --nub $ concat(filter(\x -> (length(x)>1 ))  (groupBy(\x y-> x==y) (sort(fst (unzip(score)))))) 
        where studentnames = map (\ x  ->  fst x ) score


invalidScores :: InputScore  -> [(StudentName,[(Subject,Marks,String)])]
invalidScores score =  
    validateMarksSubjects (\newlist (studentname,subjectinfo) ->  newlist++    
                        case subjectinfo of 
                            Nothing -> []
                            Just scorell -> [(studentname, (concatMap (\(subjectname,marks,validsubject,validmessage)->
                                                if(validsubject==False)
                                                    then [(subjectname,marks,validmessage)]
                                                else [] )) scorell )] ) [] score 
    


averageMarks :: InputScore -> Subject -> Float
averageMarks score subjectName =
    (fromIntegral( sum( markslist) ) / fromIntegral(length markslist))   
    where markslist = validateMarksSubjects (\newlist (_,subjectinfo) -> newlist++
                                case subjectinfo of
                                    Nothing -> []
                                    Just scorell -> (concatMap (\ (subjectname,marks,validsubject,_) -> 
                                                        if (subjectname == subjectName && validsubject==True)
                                                            then [marks]
                                                        else []) ) scorell ) [] score




standardDeviation :: InputScore -> Subject -> Float
standardDeviation score subjectName = 
    sqrt(fromIntegral(calculateXMSquare `div` (length(listsub) ) )   )  
    where averagevalue = averageMarks score subjectName                               
          listsub = validateMarksSubjects (\newlist (studentname,subjectinfo) -> newlist++
                        case subjectinfo of
                                Nothing -> []
                                Just scorell -> (concatMap (\ (subjectname,marks,validsubject,_) -> 
                                    if (subjectname == subjectName && validsubject==True)
                                        then [marks]
                                    else []) ) scorell ) [] score
          calculateXM = ( zipWith (\x y -> x-y)  listsub [round(averagevalue)..] )
          calculateXMSquare = sum $ map (^2) (calculateXM) 
    


------------------

validSubjectsStudentList :: [(Subject, [StudentName])]
validSubjectsStudentList = [("English", []),("Geography", []),("Physics", []),("Chemistry", []),("Economics", []),("Computer Science", [])]



-------------------------------------------

studentsListForExam1 :: InputScore -> [(Subject, [StudentName])]
studentsListForExam1 score = 
            map (\z -> foldl' (\ (subjectname,studentname) (x,y) ->
                                            if(x == subjectname)
                                                then (subjectname,studentname++[y])
                                            else (subjectname,studentname)   )
                                        z listsub) validSubjectsStudentList

    where listsub = validateMarksSubjects (\newlist (studentname,subjectinfo) -> newlist++
                        case subjectinfo of
                            Nothing -> []
                            Just scorell ->(concatMap (\(subjectname,_,validsubject,_)->
                                                if( validsubject == True)
                                                    then [(subjectname,studentname)]
                                                else [] )) scorell
                                            ) [] score 
                                                                                                            --[("English",["Mojo"]),("Geography",["Mojo","Captain Jack","Dexter"]),("Physics",["Mojo"]),("Chemistry",["Mojo","Captain Jack"]),("Economics",[]),("Computer Science",[])]




grouping :: InputScore -> [([Subject], [StudentName])]
grouping score  = 
    zip (subjectListbynames) namesListBysub 
    where
        listofstudent = validateMarksSubjects (\newlist (studentname,subjectinfo) -> newlist++
                        case subjectinfo of
                            Nothing -> []
                            Just scorell ->[(studentname,concatMap(\(subjectname,_,validsubject,_) -> 
                                                if (validsubject==True)
                                                    then [subjectname]
                                                else []
                                                ) scorell)]
                                            ) [] score

                                                                                                                        --[("Mojo",["English","Chemistry","Physics","Geography"]),("Captain Jack",["Chemistry","Geography"]),("Bahubali",[]),("Dexter",["Geography"])]                                            --1--[("Mojo",["English","Chemistry","Physics","Geography"]),("Captain Jack",["Chemistry","Geography"]),("Bahubali",[]),("Dexter",["Geography"])]  
        subjectStudentList = studentsListForExam1 score 
                                                                                                                            --[("English",["Mojo"]),("Geography",["Mojo","Captain Jack","Dexter"]),("Physics",["Mojo"]),("Chemistry",["Mojo","Captain Jack"]),("Economics",[]),("Computer Science",[])]       
        subjectListbynames = delete [] $ nub $ (map (\(_,z)-> z) listofstudent)                           
        namesListBysub =  nub $ (map(\(_,z)-> z) subjectStudentList)

         

