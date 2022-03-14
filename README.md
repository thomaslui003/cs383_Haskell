# Haskell383
**proj_1.hs project description:**

Compile the haskell project proj_1.hs with the test file test.txt through ghci


A formula in propositional logic can be a boolean constant (Const) with value True or False, a
boolean variable (Var) such as x1, x2, . . ., or the composition of formulas using logic connectives ¬ (Not), ∧
(And), ∨ (Or), → (Imply), and ↔ (Iff).

For a formula ϕ, a variable assignment is a mapping that maps each variable in ϕ to a truth value in
{True, False}. Given a formula ϕ and a variable assignment, the formula ϕ evaluates to a truth value based
on the following truth tables (where T stands for True and F stands for False).

For example, consider a concrete formula ϕ being x1∧¬x2. ϕ evaluates to True if the variable assignment
is x1 = True and x2 = False. Also, ϕ evaluates to False if the variable assignment is x1 = True and
x2 = True.

A formula ϕ is said to be satisfiable if there exists a variable assignment under which ϕ evaluates to True.
Otherwise, the formula is said to be unsatisfiable. In general, the satisfiability of a formula can be checked
using the truth table method. Specifically, we can list all possible variable assignments of a formula, and
then check if any variable assignment can make the formula evaluate to True.
For example, check the satisfiability of x1 ∧ ¬x2.

Here, x1 ∧ ¬x2 is satisfiable because there exists a variable assignment x1 = T and x2 = F under which
the formula evaluates to T.
As another example, check the satisfiability of ¬(x1 → (x2 → x1)).

Here, ¬(x1 → (x2 → x1)) is unsatisfiable, because there is no variable assignment that can make the
formula evaluate to T.
You need to write a Haskell program to check the satisfiability of formulas in propositional logic. The
program must be in a form that GHC can compile (i.e., you need a main). It needs to take one command-line
argument that denotes the path to the formula file. You can assume each line of the file contains a formula
to check, and the program needs to print to the console telling whether each formula is satisfiable (print
SAT) or not (print UNSAT).

Sample Input and Output:
Suppose we have a formula file called formulas.txt that contains the following two lines:
(And (Var "x1") (Not (Var "x2")))
(Not (Imply (Var "x1") (Imply (Var "x2") (Var "x1"))))
After compiling, we can run the executable and get
$ ./proj_1 formulas.txt
SAT
UNSAT




**proj_2.hs project description:**

Compile the haskell project proj_2.hs with the test file formulas.txt through ghci

Consider the following grammar G0 for formulas in propositional logic

F ormula ::= ‘T’ | ‘F’ | Ident.  
| ‘(’ F ormula ‘)’.  
| ‘!’ F ormula.  
| F ormula ‘/\’ F ormula    
| F ormula ‘\/’ F ormula     
| F ormula ‘->’ F ormula      
| F ormula ‘<->’ F ormula

Here, F ormula is the start symbol. T stands for the constant True, and F stands for the constant False.
Ident denotes variable names, starting with a lower-case letter, followed by zero or more alphanumeric
characters (letters or digits). ! denotes the logical not, /\ denotes the logical and, \/ denotes the logical or,
-> denotes the logical implication, and <-> denotes the logical iff. The precedence of different operators
(from high to low) is as follows: (), !, /\, \/, ->, <->. All binary operators are right-associative.
In this assignment, you need to write a parser in Haskell to parse strings in the language of G0. Given
such a string, the parsing result should be a value of the following type

data Prop = Const Bool
| Var String   
| Not Prop      
| And Prop Prop    
| Or Prop Prop   
| Imply Prop Prop   
| Iff Prop Prop   
deriving (Eq, Show, Read)


As indicated by the names of data constructors, Const denotes a boolean constant, Var denotes a variable,
Not denotes the logical not, And denotes the logical and, Or denotes the logical or, Imply denotes the logical
implication, and Iff denotes the logical iff. For example,

• T should be parsed into Const True.  
• t should be parsed into Var "t".   
• x1 /\ x2 should be parsed into And (Var "x1") (Var "x2"). 
• x1 /\ x2 \/ x3 should be parsed into Or (And (Var "x1") (Var "x2")) (Var "x3"). 
  
Handling Whitespaces
In general, a whitespace means a space character or a control character that is similar to a space, such as
\t, \r, \n. The complete set of whitespace characters is defined by the isSpace function from Data.Char.
When writing a grammar like G0, we can assume there are zero or more whitespace characters surrounding
each symbol in the grammar. For example, “T” is a string in the language of G0. “ T ” with preceding
and trailing whitespaces is also considered a string in the language of G0. However, we cannot assume there
is any whitespace “inside” the symbol with quotation marks in the grammar. For example, /\ should be
considered as one symbol as a whole. No whitespace is allowed between / and \, because adding whitespaces
between /\ splits it into two symbols.
You need to follow the above convention when writing a grammar. You also need to handle whitespaces
in your parser. As a hint, the token function that we learned in class can handle whitespaces.



