module Chapter3.ReversePolishNotation2 where


data Expr = ELit Float Expr
          | EMul Expr
          | EAdd Expr
          | ESubtract Expr
          | EDivide Expr
          | ENone
          deriving (Eq, Show, Ord)

expr1 :: Expr
expr1 = ELit 10.0 (ELit 20.0 (EAdd ENone))
--(True, "Valid expression", 30.0)
expr2 :: Expr
expr2 = ELit 10.0 (ELit 20.0 (ELit 30.0 (EAdd ENone)))
--(True, "Invalid expression", 0.0)

expr3 :: Expr
expr3 = ELit 10.0 (ELit 20.0 (ELit 20.0 (ESubtract (EDivide ENone))))
--(True, "Divide by zero", 0.0)






calculate :: Expr -> [Float] -> (Bool,String,Float) 
calculate e stack1 = 
    case e of
        ELit n expr -> calculate expr (( ([n]++stack1) ) )
        EAdd expr ->
            let num1 = head stack1
                list1 = tail stack1
                num2 = head list1
                list2 = tail list1
             in calculate expr ([(num1+ num2)]++list2)
        EMul expr ->
            let num1 = head stack1
                list1 = tail stack1
                num2 = head list1
                list2 = tail list1
         in calculate expr ([(num1 * num2)]++list2)
        ESubtract expr ->
            let num1 = head stack1
                list1 = tail stack1
                num2 = head list1
                list2 = tail list1
         in calculate expr ([(num1- num2)]++list2)
        EDivide expr -> 
            let num1 = head stack1
                list1 = tail stack1
                num2 = head list1
                list2 = tail list1
            in if num1 == 0
                  then (False,"cant divide by 0",num1/num2)
                else calculate expr ([(num1/num2)] ++ list2)
        ENone -> 
            let check = (length stack1 == 1 )
                in if check
                    then (True,"valid expression",head stack1)
                  else (False,"invalid expression",0)
    