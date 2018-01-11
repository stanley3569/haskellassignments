module Chapter2.Practice3 where
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
                        in (subject, fromIntegral( (foldl'(\sum y -> (sum+snd y) ) 0 listSubMarks ) `div` length(listSubMarks)  ) )


type ValidSubjects = [String]
type StudentName = String
type Marks = Int
type Validate = (Bool,String)                                                                                                   --type Validate = Bool            --(Bool,String)                   --changed for 4


validateMarksSubjects :: InputScore -> ValidSubjects -> StudentName -> Subject -> Validate
validateMarksSubjects score validsubject studentName subjectName = let validMarks = snd (head (filter (\x -> (fst x == subjectName)) (snd (head (filter (\x -> (fst x) == studentName) score)))   ))
                                                              in  if (validMarks < 0)
                                                                      then (False,"negative marks")
                                                                  else if (validMarks > 100)
                                                                        then (False,"marks higher than 100")
                                                                  else if(subjectName `elem` validsubject)
                                                                        then (True,"valid Subject")
                                                                  else (False,"unknown Subject")

--validateSubject score = foldl' (\arr x -> if (fst (validateMarksSubjects score subjects studname (fst x)))== True then arr ++ [(fst x,snd x,snd (validateMarksSubjects score [] studname (fst x)))] else arr) [] (snd (head (filter (\x -> (fst x)== studname) score)))


--checksubject :: [String] -> String -> (Bool,String)
--checksubject score validsubject subject  = let listValidSubMarks = snd(   head(filter(\x -> (fst x == validsubject))      (snd (  head(filter(\x -> (fst x) == subject)score) ))  ))
 --                                         in  if(subject `elem` validsubject)
  --                                              then (True,"Valid Subject")
   --                                           else (False,"Invalid Subject")



validateNames :: InputScore -> StudentName -> Bool                                                          --check if name is there in the result
validateNames score studentName = length (filter (\x -> (x == studentName))  (studentList1 score)) == 1

studentList1 :: InputScore -> [String]  
studentList1 score = fst (unzip(score))
                        

--xyz score = (foldl' (\arr x -> arr ++ [fst x]) [] score)


--seperating :: InputScore -> ([String],[String,Int])
--seperating score = fst (unzip(score))

--["Mojo","John Doe","Jack","John Doe","Bahubali","Rajnikant"]


--seperating2 score = snd (unzip(score))

--[[("English",84),("Chemisty",80),("Physics",95),("Geography",75)],[("Chemisty",80),("Physics",95),("Geography",75)],[("Chemisty",66),("Phsyics",33),("Geography",56)],[("Chemisty",90),("Economics",45),("Geography",56)],[("Hindi",45),("Biology",-90),("Geography",-75)],[("Tamil",110),("Biology",100),("Geography",100)]]




type DuplicateStudentList =   [String]                                        --3rd    ["John Doe","John Doe"]                                       --3rd point          
duplicateStudents score = let studentsList = studentList1 score
                              in (filter (\x -> not((validateNames score x))) studentsList) 


standardDeviation :: InputScore -> ValidSubjects -> Subject -> Float          --2nd                           --2nd point
standardDeviation score validsubject subjectName = let averagevalue = (averageMarks score validsubject subjectName)
                                                       listsub = (filter (\x -> (fst x == subjectName)) (foldl' (\arr x -> arr ++ (snd x)) [] (allValidSubjects score validsubject)))
                                                       markslist = (snd(unzip listsub))
                                                       
                                                       averageOfMarks = fromIntegral  (sum ((snd(unzip listsub))) `div` length markslist    )            -- calculateXM                      * calculateXM 
                                                       calculateXM = ( zipWith (\x y -> x-y)  markslist [averageOfMarks..] )
                                                       calculateXMSquare = sum (map (^2) (calculateXM) )
                                                       --calculateXMSquare = calculateXM * calculateXM
                                                       in sqrt(fromIntegral(calculateXMSquare `div` length(markslist) ) )
                                                       --in sum(markslist ) `div` length(markslist)
                                                      --in calculateXMSquare
                                                      


--snd (unzip [("English",84)])
--[84]

--duplicates1 :: [String] -> [String]
--duplicates1 xs = foldl (\seen x -> if x `elem` seen then seen++[x] else seen) [] xs
                              




    --(filter (\x -> (x/=)) (foldl' (\arr x -> arr ++ [fst x]) [] score))
                    --let arrstudent =  (foldl' (\arr x -> arr ++ [fst x]) [] score)
                      --  in (foldr (\x seen -> if x `elem` seen then seen else x : seen) [] arrstudent)


--type ValidStudents = [(String,[(String,Int)])] 
--validateStudent :: InputScore -> ValidStudents           ---[(String,[(String,Int)])] -> [(String,[(String,Int)])] 

-- ///validateStudent score = foldl'(\seen x -> seen ++ [x] ) [] (foldl' (\arr x -> arr ++ (fst x)) [] score)

--validateStudent score = foldl'(\seen x -> seen ++ (checkUnique [x] []) ) [] (foldl' (\arr x -> arr ++ (fst x)) [] score)

                        --foldl'(\seen x -> if x `elem` seen then seen else seen ++ [x]) [] (foldl' (\arr x -> arr ++ (fst x)) [] score)
                        --(filter (\x->  (/=))  (foldl' (\arr x -> arr ++ (fst x)) [] score) ) 
                        --let uniqueStudents =  (filter (\x -> fst x) (foldl' (\arr x -> arr ++ (fst x)) [] score)) 
                        --in  ((filter (/=x ) uniqueStudents) )




type SubjectMarks = [(String,Int)]                    

validSubjects :: InputScore -> ValidSubjects -> StudentName -> SubjectMarks                           --[(subject,marks)] ---give students valid subject and marks
validSubjects score validsubject studentname  = foldl' (\subMarksList x -> if (fst (validateMarksSubjects score validsubject studentname (fst x)))== True 
                                                                        then subMarksList ++ [(fst x,snd x)] 
                                                                  else subMarksList) [] (snd (head (filter (\x -> (fst x)== studentname) score)))
                                               

xyzz score studentname= (snd (head (filter (\x -> (fst x)== studentname) score)))
--xyzz1 score studentname= (snd (head (filter (\x -> (fst x)== studentname) score)))


type ValidStudents = [(String,[(String,Int)])] 

allValidNames :: InputScore -> ValidSubjects -> ValidStudents                                   --gives result of student with valid name,sub,marks
allValidNames score validsubject = let validStudentlist = foldl' (\arr x -> arr ++ [((fst x),(validSubjects score validsubject (fst x)) )]) [] score
                                   in filter (\x -> not ((fst x) `elem` (duplicateStudents validStudentlist))) validStudentlist


--xyz score validsubject = foldl' (\arr x -> arr ++ [((fst x),(validSubjects score validsubject (fst x)) )]) [] score


invalidSubjects :: InputScore -> ValidSubjects -> StudentName -> [(String,Int,String)]                                  --gives sub,marks,unknow sub
invalidSubjects score validsubject studentName = foldl' (\arraySubject x -> if( (fst (validateMarksSubjects  score validsubject studentName (fst x)))== False  )
                                                                              then arraySubject ++ [(fst x,snd x,snd (validateMarksSubjects score [] studentName (fst x)))] 
                                                                  else arraySubject) [] (snd (head (filter (\x -> (fst x)== studentName) score)))


--xyz score studentName = (snd ((unzip (filter (\x -> (fst x)== studentName) score))) )



type StudentsForInvalidScoreSubject = [(String, [(String,Int,String)] )]                                                            --4th point
invalidSubjectMarksStudent :: InputScore -> ValidSubjects ->  StudentsForInvalidScoreSubject                        --4         [("Mojo",[("Chemisty",80,"unknown Subject")]),("John Doe",[("Chemisty",80,"unknown Subject")]),("Captain Jack",[("Chemisty",66,"unknown Subject"),("Phsyics",33,"unknown Subject")]),("John Doe",[("Chemisty",80,"unknown Subject")]),("Bahubali",[("Hindi",45,"unknown Subject"),("Biology",-90,"negative marks"),("Geography",-75,"negative marks")]),("Dexter",[("Tamil",110,"marks higher than 100"),("Biology",100,"unknown Subject")])]                                   --[(String, [(String,Int,String)] )]
invalidSubjectMarksStudent score validsubject =  ( foldl' (\arr x -> if ((invalidSubjects score validsubject (fst x)) /= [])
                                                                 then arr ++ [(fst x,(invalidSubjects score validsubject (fst x)))]
                                                            else arr) [] score )

--seperating2 score = snd (unzip(score))

--[[("English",84),("Chemisty",80),("Physics",95),("Geography",75)],[("Chemisty",80),("Physics",95),("Geography",75)],[("Chemisty",66),("Phsyics",33),("Geography",56)],[("Chemisty",90),("Economics",45),("Geography",56)],[("Hindi",45),("Biology",-90),("Geography",-75)],[("Tamil",110),("Biology",100),("Geography",100)]]


allValidSubjects :: InputScore -> ValidSubjects -> InputScore                                               --gives valid result
allValidSubjects score validsubject = foldl' (\arr x -> arr ++ [((fst x),(validSubjects score validsubject (fst x)) )]) [] score







type StudentForSubject = [(String,[String])]                                                                                                --5th point
studentsListForExam :: ValidStudents -> ValidSubjects -> StudentForSubject                        -----5  [("English",["Mojo"]),("Geography",["Mojo","Captain Jack","Dexter"]),("Physics",["Mojo"]),("Chemistry",[]),("Economics",[]),("Computer Science",[])]
studentsListForExam score validsubject=  foldl' (\arr x -> arr ++ [(allStudentsForExam score validsubject x)]) [] validsubject





allStudentsForExam :: InputScore -> ValidSubjects -> Subject -> (Subject,[StudentName])                           --gives subject with valid students
allStudentsForExam score validsubject subjectName = foldl' (\(subjectName,subject) x ->
                                                            if (studentForExam score validsubject (fst x) subjectName)
                                                                 then (subjectName, subject ++ [fst x])
                                                            else  (subjectName,subject)) (subjectName,[]) (allValidNames score validsubject)

studentForExam :: InputScore -> ValidSubjects -> StudentName -> Subject -> Bool                            --validates if a student is ther in the course                                     
studentForExam score validsubject studentName subjectName = (filter (\x -> (fst x)==subjectName) (validSubjects score validsubject studentName)) /= []



 
groupAllStudents :: InputScore -> ValidSubjects -> StudentForSubject
groupAllStudents score validsubject =( foldl' (\arrayStudents x -> arrayStudents ++ [(allStudentsForExam score validsubject x)]) [] validsubject )





--------
{-
--allStudentsForExam1 :: InputScore -> ValidSubjects -> Subject -> ([Subject],[StudentName])                           --gives subject with valid students
allStudentsForExam1 score validsubject subjectName =groupTogether (foldl' (\(subjectName,subject) x ->
                                                                        if (studentForExam score validsubject (fst x) subjectName)
                                                                              then (subjectName , subject ++ [fst x])
                                                                        else  (subjectName,subject)) (subjectName,[]) (allValidNames score validsubject)  )

groupAllStudents1 score validsubject = ( foldl' (\arrayStudents x -> arrayStudents ++ [(allStudentsForExam1 score validsubject x)]) [] validsubject )

groupTogether xs =  [( ( []++ [fst(xs)] ) ,snd (xs) ) ]
                                                            
test1 score = (snd (unzip score) )

test2 :: [ (String,[String]) ] -> [ ([String],[String]) ]
test2 [ (x,[y]) ] = [([x],[y])] 
-}
--------




--[("English",["Saurabh Nanda"]),("Geography",["Saurabh Nanda","Jane Doe","Rajnikant"]),("Physics",["Saurabh Nanda"]),("Chemistry",[]),("Economics",[]),("Computer Science",[])]

--[(["English","Geography","Physics"],["Saurabh Nanda"]) , (["Geography"],["Saurabh Nanda","Jane Doe","Rajnikant"]) , (["Chemistry" ,"Economics","ComputerScience"],[])]




--use allValidNames --[(String,[(String,Int)])]     [(name,[(sub,marks)])] 

--allValidNames :: InputScore -> ValidSubjects -> ValidStudents                                   --gives result of student with valid name,sub,marks
--allValidNames score validsubject = let validStudentlist = foldl' (\arr x -> arr ++ [((fst x),(validSubjects score validsubject (fst x)) )]) [] score
 --                                  in filter (\x -> not ((fst x) `elem` (duplicateStudents validStudentlist))) validStudentlist




--groupxyz score validsubject = foldl'(\x y ->  ([]++fst y , []++snd ( unzip y) )   ) ([],[]) (allValidNames score validsubject)
--[("Mojo",[("English",84),("Physics",95),("Geography",75)]),("Captain Jack",[("Geography",56)]),("Bahubali",[]),("Dexter",[("Geography",100)])]

type GroupingStudentSubject = [([String],[String])]                                       ---6th point
grouping :: InputScore -> ValidSubjects -> GroupingStudentSubject
grouping score validsubject = let listofstudent = studentDetails score validsubject
                                  subjectStudentList = studentsListForExam score validsubject
                                  subjectListbynames = delete [] (nub(foldl' (\x (y,z)-> x++[z]) [] listofstudent))                           
                                  namesListBysub =  (nub(foldl'(\x (y,z)-> x++[z]) [] subjectStudentList))                                                             
                               in zip (subjectListbynames) namesListBysub  
                               --in subjectListbynames
                               --in listofstudent
                               --in subjectStudentList                                                                       
     

type StudentDetails = [(String,[String])]

studentDetails :: InputScore -> ValidSubjects -> StudentDetails 
studentDetails score validsubject = foldl' (\x (studentName,subjectMarks) -> x++[(studentName,(foldl' (\x (subjectName,marks) -> x++[subjectName]) [] subjectMarks))] ) [] (allValidNames score validsubject)




