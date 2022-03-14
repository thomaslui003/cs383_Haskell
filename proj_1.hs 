{-# OPTIONS_GHC -Wno-incomplete-patterns #-}
import System.IO ( hClose, openFile, hGetLine, IOMode(ReadMode), hGetContents )
import System.Environment (getProgName, getArgs)
import Control.Exception (handle)
import GHC.Exts.Heap (GenClosure(key, var))



main:: IO ()
main = do
    args <- getArgs 
    --handle not needed
    handle <- openFile (head args) ReadMode
    contents <- hGetLine handle

    thefile <- readFile (head args)
    let listOfFormulas = lines thefile


    startEqnEval listOfFormulas
    
    
    hClose handle




-- This startEqnEval function perform all of the formula evaluation by reading in a list of string of formula. And with each equation, 
-- the head x String, will use the derived read function of custom data type structure and read it as a Equation type variable. Then, 
-- with the same string, we use the retrieveVariable function to extract all of the Var String variable ("x1"...) in list of string form.
-- Then the list may contain duplicate of the same Var string. we use the filtDuplVariable function to filter out duplicated string and reverse
-- the list for debugging purpose. With the list of Var string, we then use the getCombination function to get all of the possible True/False value
-- combination that can be mapped to the given Var variables. (eg. [[("x1",True),("x2",True)],[("x1",True),("x2",False)],[("x1",False),("x2",True)],[("x1",False),("x2",False)]])
-- Then, the list of list of tuple mapped value will be used to put into the iterateAllCombination function with the current formula and empty list. And, 
-- the result of each list of tuple will be evaluated with the current formula to produce a list of bool value that show which set of T/F value assigned to
-- the Var variable evaluate to true or false. Then, the list of bool value will be used to check the overall satisfiability of the formula and 
-- print SAT or UNSAT accordingly. After the current formula evaluated, it will recursively evaluate the rest of the file's formula equation and 
-- output its satisfiability.
startEqnEval :: [String]  -> IO()
startEqnEval (x:xs) = do
  let currentEqn = read x ::  Equation
  let listofVar3 = retrieveVariable x
  let removedupl3 = filtDuplVariable listofVar3
  let correctOrdering3 = reversinglist removedupl3
  let combinationOfBool2 = getCombination correctOrdering3
  let iterateComb = iterateAllCombination combinationOfBool2 currentEqn []
  let satOrUnsat2 = checkSatisfaction iterateComb
  if satOrUnsat2 then putStr ("SAT" ++ "\n") else putStr ("UNSAT" ++ "\n")
  startEqnEval xs
startEqnEval [] = putStr ""



--the custom Equation type data structure with Var, And, Or, Not, Imply, and Iff type constructor.
data Equation = Var String
  |Iff Equation Equation
  |And Equation  Equation
  |Imply Equation Equation
  |Or Equation  Equation 
  |Not Equation 
  deriving (Show, Read)



-- function that output all of the combination of the number of variable given (eg. with input of x1 x2 then it will have 2^2 combination)
-- where each variable can have True or False and put each combination into a list of tuple that have each Var string value map to a True or
-- or false value. 
getCombination :: [String] -> [[(String, Bool)]]
getCombination (x:xs) = [(x,True):i | i <- getCombination xs] ++ [(x,False):i | i <- getCombination xs]
getCombination [] = [[]]
-- now have all combination of x1 x2 true and false in a list of list form 
-- now output: [[("x1",True),("x2",True)],[("x1",True),("x2",False)],[("x1",False),("x2",True)],[("x1",False),("x2",False)]]
--with input of ["x1","x2"]



-- A function that take in a string (the ) and scan until we encounter the first " quotation mark and retrieve the content inside until
-- the next quotation mark appears. And, this String will be added to the list of string. In general, this function is a substring extraction
-- function that get all the value of Var variable in string form. 
retrieveVariable :: String -> [String]
retrieveVariable = scanningline

scanningline [] = []
scanningline ('"':xs) = 
    let (k,rest) = scanningline' xs 
    in k:scanningline rest
scanningline (_:xs) = scanningline xs

scanningline' [] = ([], []) 
scanningline' ('"':xs) = ([], xs)
scanningline' (x:xs) = let (k,rest) = scanningline' xs in (x:k, rest)



-- The truth table evaluation function that take in the current formula and take the list of tuple with mapped true and false value and 
-- output a true or false bool value that determine whether the formula is satisfiable. 
-- example run would be: Equation = (Or (Var "x1") (Var "x2")) and list of tuple = [("x1",True),("x2",True)]
-- then it uses the Or evaluation that recursively call itself on left and right side with x = x1 and y = x2 and a OR operator
-- within the left and right side recursive call, it takes the Var value = x1 and the first set of tuple and if the var value matched 
-- the tuple x1 string then we map it to equal to True else we use the next tuple to match the Var value to map the True or False value.
-- With the Bool value mapped onto the formula, the OR operator can determine if the overall formula is True or False. 
tableEvaluation :: Equation -> [(String, Bool)] -> Bool
tableEvaluation (Var v) ((x1,x2):xs)
    | v == x1 = x2
    | otherwise = tableEvaluation (Var v) xs
tableEvaluation (And x y) k = tableEvaluation x k && tableEvaluation y k
tableEvaluation (Or x y) k = tableEvaluation x k || tableEvaluation y k
tableEvaluation (Not x) k = not (tableEvaluation x k)
tableEvaluation (Imply x y) k
  | (tableEvaluation x k == True) && (tableEvaluation y k == False) = False
  | otherwise = True
tableEvaluation (Iff x y ) k
  | (tableEvaluation x k == True) && (tableEvaluation y k == False) = False
  | (tableEvaluation x k == False) && (tableEvaluation y k == True) = False
  | otherwise = True




-- A function that takes in a list of list of tuple of Var value and bool value which include all combination of true/false value from
-- the number of Var string variable. Then it also take in the current equation that needs to be evaluated with truth table and as well 
-- as take in an empty list of bool type. The function recursively take the head x (a list of tuple eg. (x1,T),(x2,T)) and with those 
-- True or False value and call the tableEvaluation function with the equation type formula and the list to get an Bool value result. At
-- the end of each set of evaluation (list of tuple), the bool result will be added to a final list of bool result (b variable) for later
-- formula satisfaction check. 
iterateAllCombination:: [[(String, Bool)]] -> Equation -> [Bool] -> [Bool]
iterateAllCombination [] _ _ = []
iterateAllCombination (x:xs) a b = b ++ (tableEvaluation a x) : iterateAllCombination xs a b




--A function that takes in a list of bool value from the result of all bool value (True/False) combination in Var variable (x1...xn) and use elem to check if 
-- the list of bool exist a true result to be a satisfiable formula.
--check for true value with the list of bool value after evaluating the satisfaction of the equation
checkSatisfaction:: [Bool] -> Bool
checkSatisfaction b = True `elem` b



-- Function takes in a list of string (eg. ["x1","x2","x3","x2"]) and remove the "x2" as it is duplicated. it is done by taking
-- the first string and call the helper function to check recursively if the rest of the list have the same string. If it does
-- drop the duplicated variable in the then function after elem.  
filtDuplVariable :: Eq x => [x] -> [x]
filtDuplVariable [] = [] 
filtDuplVariable (x:xs) = filtDuplHelper [x] xs

filtDuplHelper :: Eq a => [a] -> [a] -> [a]
filtDuplHelper x [] = x 
filtDuplHelper [] (x:xs) = filtDuplHelper [x] xs
filtDuplHelper x (a:xs) = 
    if a `elem` x   
        then filtDuplHelper x xs 
        else filtDuplHelper (a:x) xs 



--the filtDuplVariable function output a list of reverse order of the Var string value therefore, the reverse function 
--fixes that for better result viewing or checking for error as the getCombination function does not care about order of 
--the var string (only the distinctness of Var string matter)
reversinglist :: [a] -> [a]
reversinglist [] = []
reversinglist (x:xs) = reversinglist xs ++ [x]

