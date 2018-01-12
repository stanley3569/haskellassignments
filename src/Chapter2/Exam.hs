module Chapter2.Exam1 where
import Data.List
import Data.Char


score :: [(String,[(String,Int)])]
score=[ ("Mojo", [("English", 84), ("Chemisty", 80), ("Physics", 95), ("Geography", 75)])
    , ("John Doe", [("Chemisty", 80), ("Physics", 95), ("Geography", 75)])
    , ("Captain Jack", [("Chemisty", 66), ("Phsyics", 33), ("Geography", 56)]) -- Note the spelling error in "Physics"
    , ("John Doe", [("Chemisty", 90), ("Economics", 45), ("Geography", 56)]) -- Note that "John Doe" is a repeat entry
    , ("Bahubali", [("Hindi", 45), ("Biology", -90), ("Geography", -75)]) -- Note the negative marks in "Biology" & "Geography"
    , ("Dexter", [("Tamil", 110), ("Biology", 100), ("Geography", 100)]) -- Note that marks in "Tamil" are greater than 100
    ]

validsubject :: [String]
validsubject = ["English", "Geography", "Physics", "Chemistry", "Economics", "Computer Science"]

type InputScore = [(String,[(String,Int)])]
type Subject = String
type SubjectAverage = (String,Float)     

averageMarks :: InputScore ->ValidSubjects -> Subject -> SubjectAverage             --1st
averageMarks score validsubject subject =let listSubMarks =  (filter (\x -> (fst x == subject)) (foldl' (\arr x -> arr ++ (snd x)) [] (allValidNames score validsubject) ) )
                                    in (subject,fromIntegral( sum( snd( unzip (listSubMarks) ) )   `div` length(listSubMarks) )     )                          ---1.0                --(foldl'(\sum y -> (sum+snd y) ) 0 listSubMarks )                   

                                    --in (subject, fromIntegral( (foldl'(\sum y -> (sum+snd y) ) 0 listSubMarks ) `div` length(listSubMarks)  ) )

xyz9 score validsubject = (foldl' (\arr x -> arr ++ (snd x)) [] (allValidNames score validsubject) )
--[("English",84),("Physics",95),("Geography",75),("Geography",56),("Geography",100)]

xyz10 score validsubject = ( map snd (allValidNames score validsubject)   )


type ValidSubjects = [String]
type StudentName = String
type Marks = Int
type Validate = (Bool,String)                                                                                                   --type Validate = Bool            --(Bool,String)                   --changed for 4


validateMarksSubjects :: InputScore -> ValidSubjects -> StudentName -> Subject -> Validate
validateMarksSubjects score validsubject studentName subjectName = 
      let validMarks = snd (head (filter (\x -> (fst x == subjectName)) (head (map (snd) (filter(\x -> (fst x) == studentName) score) ) )   ))              --1.0
            in  if (validMarks < 0)
                      then (False,"negative marks")
                else if (validMarks > 100)
                      then (False,"marks higher than 100")
                else if(subjectName `elem` validsubject)
                      then (True,"valid Subject")
                else (False,"unknown Subject")

validateNames :: InputScore -> StudentName -> Bool                                                          --check if name is there in the result
validateNames score studentName = (length (filter (\x -> (x == studentName)) (studentList1 score)) == 1)

studentList1 :: InputScore -> [String]  
studentList1 score = fst (unzip(score))
   


xyz score studentname = (snd (head (filter (\x -> (fst x) == studentname) score)))              --test
--[("English",84),("Chemisty",80),("Physics",95),("Geography",75)]

xyz1 score studentname = (filter(\x -> (fst x) == studentname) score)                                 --test
--[("Mojo",[("English",84),("Chemisty",80),("Physics",95),("Geography",75)])]

xyz2 score studentname =head (map (snd) (filter(\x -> (fst x) == studentname) score) )                --test
--[("English",84),("Chemisty",80),("Physics",95),("Geography",75)]





type DuplicateStudentList =   [String]                                        --3rd    ["John Doe","John Doe"]                                       --3rd point          
duplicateStudents score = let studentsList = studentList1 score
                              in (filter (\x -> not((validateNames score x))) studentsList) 


standardDeviation :: InputScore -> ValidSubjects -> Subject -> Float          --2nd                           --2nd point
standardDeviation score validsubject subjectName = let averagevalue = (averageMarks score validsubject subjectName)
                                                       listsub = (filter (\x -> (fst x == subjectName)) (concat ( snd (unzip(allValidSubjects score validsubject) ) ) ))
                                                       markslist = (snd(unzip listsub))                                                      
                                                       averageOfMarks = fromIntegral  (sum ((snd(unzip listsub))) `div` length markslist    )            -- calculateXM                      * calculateXM 
                                                       calculateXM = ( zipWith (\x y -> x-y)  markslist [averageOfMarks..] )
                                                       calculateXMSquare = sum (map (^2) (calculateXM) )
                                                       in sqrt(fromIntegral(calculateXMSquare `div` length(markslist) ) )
                                                      --in listsub



xyz4 score validsubject = (foldl' (\arr x -> arr ++ (snd x)) [] (allValidSubjects score validsubject))                  --test
--[("English",84),("Physics",95),("Geography",75),("Physics",95),("Geography",75),("Geography",56),("Physics",95),("Geography",75),("Geography",100)]

xyz14 score validsubject =(concat ( snd (unzip(allValidSubjects score validsubject) ) ) )
--[("English",84),("Physics",95),("Geography",75),("Physics",95),("Geography",75),("Geography",56),("Physics",95),("Geography",75),("Geography",100)]




type SubjectMarks = [(String,Int)]                    

validSubjects :: InputScore -> ValidSubjects -> StudentName -> SubjectMarks                           --[(subject,marks)] ---give students valid subject and marks
validSubjects score validsubject studentname  = foldl' (\subMarksList x -> 
                                                if (fst (validateMarksSubjects score validsubject studentname (fst x)))== True 
                                                      then subMarksList ++ [(fst x,snd x)] 
                                                else subMarksList) [] (head (map (snd) (filter(\x -> (fst x) == studentname) score) ) )
   
{-}
validSubject1 score validsubject studentname = let resultName =( ( ( (filter(\x -> (fst x) == studentname) score) ) )  )
                                                   subMarksList = []
                                                   validresultName = if( (fst (validateMarksSubjects score validsubject studentname  (concat(fst (unzip (resultName) )))) ) == True )
                                                                        then subMarksList ++ [(fst (unzip(resultName) ),(snd (unzip(resultName) )  ))] 
                                                                     else subMarksList
                                                   in validresultName
-}

xyz111 score studentname = (snd (head (filter (\x -> (fst x)== studentname) score)))
xyz111t score studentname = (head (map (snd) (filter(\x -> (fst x) == studentname) score) ) )


type ValidStudents = [(String,[(String,Int)])] 

allValidNames :: InputScore -> ValidSubjects -> ValidStudents                                   --gives result of student with valid name,sub,marks
allValidNames score validsubject = let validStudentlist = map(\x -> (fst x, (validSubjects score validsubject (fst x)))) score
                                   in filter (\x -> not ((fst x) `elem` (duplicateStudents validStudentlist))) validStudentlist





xyz6 score validsubject = foldl' (\arr x -> arr ++ [((fst x),(validSubjects score validsubject (fst x)) )]) [] score                --test
--[("Mojo",[("English",84),("Physics",95),("Geography",75)]),("John Doe",[("Physics",95),("Geography",75)]),("Captain Jack",[("Geography",56)]),("John Doe",[("Physics",95),("Geography",75)]),("Bahubali",[]),("Dexter",[("Geography",100)])]


xyz6t score validsubject = map(\x -> (fst x, (validSubjects score validsubject (fst x)))) score
--[("Mojo",[("English",84),("Physics",95),("Geography",75)]),("John Doe",[("Physics",95),("Geography",75)]),("Captain Jack",[("Geography",56)]),("John Doe",[("Physics",95),("Geography",75)]),("Bahubali",[]),("Dexter",[("Geography",100)])]

invalidSubjects :: InputScore -> ValidSubjects -> StudentName -> [(String,Int,String)]                                  --gives sub,marks,unknow sub
invalidSubjects score validsubject studentName = foldl' (\arraySubject x -> if( (fst (validateMarksSubjects  score validsubject studentName (fst x)))== False  )
                                                                              then arraySubject ++ [(fst x,snd x,snd (validateMarksSubjects score [] studentName (fst x)))] 
                                                                  else arraySubject) [] (snd (head (filter (\x -> (fst x)== studentName) score)))




type StudentsForInvalidScoreSubject = [(String, [(String,Int,String)] )]                                                            --4th point
invalidSubjectMarksStudent :: InputScore -> ValidSubjects ->  StudentsForInvalidScoreSubject                        --4         [("Mojo",[("Chemisty",80,"unknown Subject")]),("John Doe",[("Chemisty",80,"unknown Subject")]),("Captain Jack",[("Chemisty",66,"unknown Subject"),("Phsyics",33,"unknown Subject")]),("John Doe",[("Chemisty",80,"unknown Subject")]),("Bahubali",[("Hindi",45,"unknown Subject"),("Biology",-90,"negative marks"),("Geography",-75,"negative marks")]),("Dexter",[("Tamil",110,"marks higher than 100"),("Biology",100,"unknown Subject")])]                                   --[(String, [(String,Int,String)] )]
invalidSubjectMarksStudent score validsubject =  ( foldl' (\arr x -> if ((invalidSubjects score validsubject (fst x)) /= [])
                                                                 then arr ++ [(fst x,(invalidSubjects score validsubject (fst x)))]
                                                            else arr) [] score )



allValidSubjects :: InputScore -> ValidSubjects -> InputScore                                               --gives valid result
allValidSubjects score validsubject = map(\x -> (fst x, (validSubjects score validsubject (fst x)))) score
      --foldl' (\arr x -> arr ++ [((fst x),(validSubjects score validsubject (fst x)) )]) [] score



xyz7 score validsubject = foldl' (\arr x -> arr ++ [((fst x),(validSubjects score validsubject (fst x)) )]) [] score
--[("Mojo",[("English",84),("Physics",95),("Geography",75)]),("John Doe",[("Physics",95),("Geography",75)]),("Captain Jack",[("Geography",56)]),("John Doe",[("Physics",95),("Geography",75)]),("Bahubali",[]),("Dexter",[("Geography",100)])]
 


type StudentForSubject = [(String,[String])]                                                                                                --5th point
studentsListForExam :: ValidStudents -> ValidSubjects -> StudentForSubject                        -----5  [("English",["Mojo"]),("Geography",["Mojo","Captain Jack","Dexter"]),("Physics",["Mojo"]),("Chemistry",[]),("Economics",[]),("Computer Science",[])]
studentsListForExam score validsubject=  foldl' (\arr x -> arr++[allStudentsForExam score validsubject x]) [] validsubject



xyz11 score validsubject = foldl' (\arr x -> arr++[allStudentsForExam score validsubject x]) [] validsubject
--[("English",["Mojo"]),("Geography",["Mojo","Captain Jack","Dexter"]),("Physics",["Mojo"]),("Chemistry",[]),("Economics",[]),("Computer Science",[])]



allStudentsForExam :: InputScore -> ValidSubjects -> Subject -> (Subject,[StudentName])                           --gives subject with valid students
allStudentsForExam score validsubject subjectName = foldl' (\(subjectName,subject) x ->
                                                            if (studentForExam score validsubject (fst x) subjectName)
                                                                 then (subjectName, subject++[fst x])
                                                            else  (subjectName,subject)) (subjectName,[]) (allValidNames score validsubject)

--allStudentsForExam1 :: InputScore -> ValidSubjects -> Subject -> (Subject,[StudentName])                           --gives subject with valid students
--allStudentsForExam1 score validsubject subjectName = let validnames = (allValidNames score validsubject)
  --                                                       subjectcheck = unzip (validnames)
    --                                                  in subjectcheck


--allValidNames score validsubject
--[("Mojo",[("English",84),("Physics",95),("Geography",75)]),("Captain Jack",[("Geography",56)]),("Bahubali",[]),("Dexter",[("Geography",100)])]



-- *Chapter2.Exam1> allStudentsForExam score validsubject "English"
--("English",["Mojo"])
-- *Chapter2.Exam1> allStudentsForExam score validsubject "Geography"
--("Geography",["Mojo","Captain Jack","Dexter"])
-- *Chapter2.Exam1> allStudentsForExam score validsubject "Physics"
--("Physics",["Mojo"])

--xyz201 score validsubject subjectName = concat (map ((snd)) (allValidNames score validsubject) )


studentForExam :: InputScore -> ValidSubjects -> StudentName -> Subject -> Bool                            --validates if a student is ther in the course                                     
studentForExam score validsubject studentName subjectName = (filter (\x -> (fst x)==subjectName) (validSubjects score validsubject studentName)) /= []


type GroupingStudentSubject = [([String],[String])]                                       ---6th point
grouping :: InputScore -> ValidSubjects -> GroupingStudentSubject
grouping score validsubject = let listofstudent = studentDetails score validsubject
                                  subjectStudentList = studentsListForExam score validsubject
                                  subjectListbynames = delete [] (nub(foldl' (\x (y,z)-> x++[z]) [] listofstudent))                           
                                  namesListBysub =  (nub(foldl'(\x (y,z)-> x++[z]) [] subjectStudentList))                                                             
                               in zip (subjectListbynames) namesListBysub  
                                                             
     

type StudentDetails = [(String,[String])]

studentDetails :: InputScore -> ValidSubjects -> StudentDetails 
studentDetails score validsubject = map(\x -> ( fst(x),head[( fst ( unzip (snd(x)) ) )] ) ) (allValidNames score validsubject)                                                                        --foldl' (\x (studentName,subjectMarks) -> x++[(studentName,(foldl' (\x (subjectName,marks) -> x++[subjectName]) [] subjectMarks))] ) [] (allValidNames score validsubject)
--[("Mojo",["English","Physics","Geography"]),("Captain Jack",["Geography"]),("Bahubali",[]),("Dexter",["Geography"])]











xyz16 score validsubject =map(\x -> ( fst(x),head[(fst (unzip (snd(x) )) )] )       ) (allValidNames score validsubject)
--[("Mojo",["English","Physics","Geography"]),("Captain Jack",["Geography"]),("Bahubali",[]),("Dexter",["Geography"])]
--foldl' (\x (studentName,subjectMarks) -> x++[(studentName,(foldl' (\x (subjectName,marks) -> x++[subjectName]) [] subjectMarks))] ) [] (allValidNames score validsubject)

--concatMap (\x -> [x+2,x]) [1,2,3]
--[3,1,4,2,5,3]

test2 score = map (fst) score
--["Mojo","John Doe","Captain Jack","John Doe","Bahubali","Dexter"]

test3 score = (map (snd) score )
--[[("English",84),("Chemisty",80),("Physics",95),("Geography",75)],[("Chemisty",80),("Physics",95),("Geography",75)],[("Chemisty",66),("Phsyics",33),("Geography",56)],[("Chemisty",90),("Economics",45),("Geography",56)],[("Hindi",45),("Biology",-90),("Geography",-75)],[("Tamil",110),("Biology",100),("Geography",100)]]

