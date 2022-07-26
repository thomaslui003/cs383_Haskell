{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Redundant bracket" #-}
{-# HLINT ignore "Use const" #-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Redundant bracket" #-}
{-# LANGUAGE TemplateHaskell #-}
{-# OPTIONS_GHC -Wno-incomplete-patterns #-}

import Control.Applicative hiding (Const)
import Data.Char
import System.Directory (findExecutablesInDirectories)
import System.Directory.Internal.Prelude

-- main function get the name of the file as args and read the file into a list of string which can be put into the startEqnEval function
-- and start the process of parsing each of the formula.
main:: IO ()
main = do
    args <- getArgs 

    thefile <- readFile (head args)
    let listOfFormulas = lines thefile
    startEqnEval (listOfFormulas)


-- startEqnEval function take in a list of string (list of the formula from text file) and use parseFormula function to parse it into 
-- the G2 grammar based formula. If the currentFormula is parsed to be not an empty list [] then it will print the formula to the console
-- else it will print Parse Error to the console. The function will end after attempting parsing all of the text file's formula.
startEqnEval :: [String]  -> IO()
startEqnEval (x:xs) = do
    let currentFormula = parseFormula x
    if (currentFormula /= []) then putStr (currentFormula ++ "\n") else putStr ("Parse Error" ++ "\n") 
     <|> if (currentFormula == "Parser Error") then putStr ("Parse Error" ++ "\n") else putStr ("") 
    startEqnEval xs
startEqnEval [] = putStr ""



data Prop = Const Bool
 | Var String
 | Not Prop
 | And Prop Prop
 | Or Prop Prop
 | Imply Prop Prop
 | Iff Prop Prop
 deriving (Eq, Show, Read)


-- G0 to G1
--Formula := Formula '<->' Formula | ImpTerm
--ImpTerm := ImpTerm '->' ImpTerm | OrTerm
--OrTerm := OrTerm '\/' OrTerm | AndTerm
--AndTerm := AndTerm '/\' AndTerm | NotTerm
--NotTerm := '!' Formula | Factor
--Factor :=  '(' Formula ')' | 'T' | 'F' | Ident


-- G1 to G2
--Formula ::= ImpTerm '<->' Formula | ImpTerm
--ImpTerm ::= OrTerm '->' ImpTerm | OrTerm
--OrTerm ::= AndTerm '\/' OrTerm | AndTerm
--AndTerm ::= NotTerm '/\' AndTerm | NotTerm
--NotTerm ::= '!' Formula | Factor
--Factor ::=  '(' Formula ')' | 'T' | 'F' | Ident

-- newtype allows Parser to be made into instances of classes
newtype Parser a = P(String -> [(a, String)])

-- parse function applies the parser to an input of type string
parse :: Parser a -> String -> [(a,String)]
parse (P p) input = p input

--A parsing primitive function that fails if input is empty, otherwise return first character as result
item :: Parser Char
item = P (\input -> case input of
    [] -> []
    (x:xs)-> [(x,xs)])

-- An instance Functor Parser - Parser made into a functor which allow fmap apply function to reult value of parser if it succeeds
instance Functor Parser where
    --fmap :: (a->b) -> Parser a -> Parser b
    fmap f p = P (\input -> case parse p input of
                [] -> []
                [(v,out)] -> [(f v, out)])

-- An instance Applicative Parser - Parser made into an applicative which allow pure which turn value into parser that succeed 
-- with the value and <*> which applies to parser that return function f and a parser that return value v to a parser that return f v.
--And it only succeeds if all components succeed.
instance Applicative Parser where
    pure v = P (\input -> [(v,input)])
    pf <*> px = P (\input -> case parse pf input of
        [] -> []
        [(f,out)] -> parse (fmap f px) out)

-- An instance Monad Parser - Parser made into a monad which allows p >>= f fails if parser p fails on the input and 
-- p >>= f otherwise applies f to result v to produce another parser f v, which is then applied to the output string out (of parser p) to give the final result
instance Monad Parser where
    p >>= f = P (\input -> case parse p input of
        [] -> []
        [(v,out)] -> parse (f v) out)

-- instance of Alternative Parser - Parser made into an instance of Alternative that have empty and <|> function. Empty returns a parser that always fails
-- <|> returns the first parser if it succeeds on the input; otherwise, returns the second parser on the same input
instance Alternative Parser where
    empty = P (\input -> [])
    p <|> q = P (\input -> case parse p input of
        [] -> parse q input
        [(v, out)] -> [(v,out)])

-- sat function allows for a single characters that satisfy predicate p to made into Parser Char
sat :: (Char -> Bool) -> Parser Char
sat p = do x <- item
           if p x then return x else empty

char :: Char -> Parser Char
char x = sat (== x)


space :: Parser ()
space = do many (sat isSpace)
           return ()
-- token function that eliminate all of the spaces. And used the space and sat function to check if the element is a whitespace
token :: Parser a -> Parser a
token p = do space
             v <- p
             space
             return v


-- pretoken function only tackle all of the whitespaces before the character and cut them out. eg. "   x1" -> "x1"
pretoken ::  Parser a -> Parser a
pretoken p = do space
                v <- p
                return v

-- posttoken function only tackle all of the whitespaces after the character and cut them out. eg. "x1    " -> "x1"
posttoken ::  Parser a -> Parser a
posttoken p = do 
                 v <- p
                 space
                 return v

-- step 4 of the assignment
-- constant function that can parse "T" or "F" of the formula. If the item function returned a "T" or "F", it will return either Const True or Const False (data type of Prop) 
-- if the x doesn't equal to T or F then it will return empty as parser.
constant :: Parser Prop 
constant = do x <- token item 
              if (x == 'T') then return (Const True) else empty
               <|> if (x == 'F') then return (Const False) else empty 

--Some parser fuctions based on predicates from Data.char
digit :: Parser Char 
digit = sat isDigit

lower :: Parser Char 
lower = sat isLower

alphanum :: Parser Char 
alphanum = sat isAlphaNum

--string function parsing primitives of specific string
string :: String -> Parser String
string [] = return []
string (x:xs) = do char x 
                   string xs 
                   return (x:xs)

-- symbol function for parsing specific symbol
symbol :: String -> Parser String 
symbol xs = token (string xs)



-- var function that parse the variable name that follow the condition of the name starting with a lower case letter followed by zero or
-- or more alphanumeric characters. pretoken and posttoken function is used to remove the whitespaces before and after the variable name.
var :: Parser Prop 
var = do x <- pretoken lower 
         xs <- posttoken (many alphanum)
         return (Var (x:xs) )

--formula function that take in a formula string (e.g., x1 /\ x2) as input and generates a string as output representing the parsing result. 
-- The function evaluates all possible formula defined by the G2 grammar. (step 6 of assignment)
formula :: Parser Prop 
formula = do t <- impTerm
             symbol "<->"
             e <- formula
             return (Iff t e)
           <|> impTerm

impTerm :: Parser Prop
impTerm = do t <- orTerm
             symbol "->"
             e <- impTerm
             return (Imply t e)
            <|> orTerm

orTerm :: Parser Prop
orTerm = do t <- andTerm
            symbol "\\/"
            e <- orTerm
            return (Or t e)
           <|> andTerm

andTerm :: Parser Prop
andTerm = do t <- notTerm
             symbol "/\\"
             e <- andTerm
             return (And t e)
            <|> notTerm

notTerm :: Parser Prop
notTerm = do symbol "!"
             t <- formula
             return (Not t)
            <|> factor

factor :: Parser Prop
factor = do symbol "("
            t <- formula
            symbol ")"
            return t
           <|> constant
           <|> var
           

-- parsingFormula function that take in string and used the parse function together with the formula function and parse the string into
-- a formula. If the parse function return an empty list [] then it will return Parser Error as string else if it's not empty, it will 
-- use the extracting function and get the Prop type formula and return it. 
parseFormula :: String -> String 
parseFormula a = do
    let tempt = parse (formula) a
    if (tempt /= empty) then extracting tempt else "Parser Error"


-- extracting function that take in [(Prop, String)] and extract the x1 which is the Prop type formula and return it as String.
extracting :: [(Prop, String)]  -> String 
extracting ((x1,x2):xs) = show x1 


