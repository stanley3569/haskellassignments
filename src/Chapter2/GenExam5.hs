module Chapter2.GenExam5 where
import Data.List
import Data.Char


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



validateMarksSubjects ::(b->  (StudentName,Bool,[(Subject,Marks,Bool,String)]) -> b ) ->b -> InputScore -> b
validateMarksSubjects testfunc accumulator score= 
    let dupicatenames = (duplicateStudents score)
        --newscore = filter (\(x,y)-> x `notElem` dupicatenames) score
        validMarks = validateMarks score
        newscore =  map(\(studentname,subjectinfo) -> 
                            if (studentname `elem` dupicatenames) 
                                then (studentname,False,subjectinfo) 
                            else (studentname,True,subjectinfo)) validMarks
    in foldl' (\x y -> testfunc x y) accumulator newscore 



type DuplicateStudentList =  [String]                                                                                         
duplicateStudents :: InputScore -> DuplicateStudentList
duplicateStudents score = 
    let studentnames = map (\ x  ->  fst x ) score
        duplicatenames = concatMap(\x -> if(length x > 1) then [head x] else [] ) (group(sort studentnames) )
                        --nub $ concat(filter(\x -> (length(x)>1 ))  (groupBy(\x y-> x==y) (sort(fst (unzip(score)))))) 
        in duplicatenames


invalidScores :: InputScore  -> [(StudentName,[(Subject,Marks,String)])]
invalidScores score =  
    let invalidData3 = validateMarksSubjects (\newlist (studentname,validstudent,subjectinfo) ->                         
                               newlist++[(studentname, (concatMap (\(subjectname,marks,validsubject,validmessage)->
                                        if(validsubject==False)
                                            then [(subjectname,marks,validmessage)]
                                        else [] )) subjectinfo)] ) [] score 
        in invalidData3


averageMarks :: InputScore -> Subject -> Float
averageMarks score subjectName =
    let markslist = validateMarksSubjects (\newlist (studentname,validstudent,subjectinfo) ->
                                newlist++(concatMap (\(subjectname,marks,validsubject,validmessage)->
                                    if(validstudent==True && subjectname==subjectName && validsubject == True)
                                        then [marks]
                                    else [] )) subjectinfo
                                ) [] score 
        averageSubject =  ( fromIntegral( sum( markslist) ) / fromIntegral(length markslist))                                          
     in averageSubject



standardDeviation :: InputScore -> Subject -> Float
standardDeviation score subjectName = 
    let averagevalue = averageMarks score subjectName                               
        listsub = validateMarksSubjects (\newlist (studentname,validstudent,subjectinfo) ->
                        newlist++(concatMap (\(subjectname,marks,validsubject,validmessage)->
                            if(validstudent==True && subjectname==subjectName && validsubject == True)
                                then [marks]
                            else [] )) subjectinfo
            ) [] score 
        calculateXM = ( zipWith (\x y -> x-y)  listsub [round(averagevalue)..] )
        calculateXMSquare = sum (map (^2) (calculateXM) )
    in sqrt(fromIntegral(calculateXMSquare `div` (length(listsub) ) )   )  


------------------

validSubjectsStudentList :: [(Subject, [StudentName])]
validSubjectsStudentList = [("English", []),("Geography", []),("Physics", []),("Chemistry", []),("Economics", []),("Computer Science", [])]

{-not working}
studentsListForExam :: InputScore -> [(Subject, [StudentName])]
studentsListForExam score = 
    let listsub =validateMarksSubjects (\newlist (studentname,validstudent,subjectinfo) ->
                    newlist++(concatMap (\(subjectname,marks,validatesubject,validmessage)-> 
                        if(validstudent==True && validatesubject == True )
                            then map (\(accsubjectname,accstudentnames) -> 
                                                if(accsubjectname == subjectname)
                                                    then (accsubjectname,[studentname]++accstudentnames)                   --studentname:accstudentnames
                                                else (accsubjectname,accstudentnames)  ) newlist
                        else [] )) subjectinfo
                 ) validSubjectsStudentList score 
                    

    in listsub
-}


-------------------------------------------

studentsListForExam1 :: InputScore -> [(Subject, [StudentName])]
studentsListForExam1 score = 
    let listsub =validateMarksSubjects (\newlist (studentname,validstudent,subjectinfo) ->
                        newlist++(concatMap (\(subjectname,marks,validsubject,validmessage)->
                            if(validstudent==True &&validsubject == True)
                                then [(subjectname,studentname)]
                            else [] )) subjectinfo
            ) [] score 
--[("English","Mojo"),("Chemistry","Mojo"),("Physics","Mojo"),("Geography","Mojo"),("Chemistry","Captain Jack"),("Geography","Captain Jack"),("Geography","Dexter")]                   
        studentlist = map (\x -> foldl (\ (subjectname,studentname) (x,y) ->
                                            if(x == subjectname)
                                                then (subjectname,studentname++[y])
                                            else (subjectname,studentname)   )
                                        x listsub) validSubjectsStudentList
    in studentlist
--[("English",["Mojo"]),("Geography",["Mojo","Captain Jack","Dexter"]),("Physics",["Mojo"]),("Chemistry",["Mojo","Captain Jack"]),("Economics",[]),("Computer Science",[])]






