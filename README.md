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






