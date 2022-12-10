{-
 - Author: Cameron Miskell, cmiskell2020@my.fit.edu
 - Author: Luke Bucher, lbucher2017@my.fit.edu
 - Course: CSE 4250, Fall 2022
 - Project: Proj4, Tautology Checker
 - Language implementation: GHC version 9.4.2
 -}

-- Style Tips:
-- Don't leave trailing white space in your code
-- Don't use tabs
-- Aim to keep lines under 80 characters
-- Use the CamelCase variable naming convention
-- Don't use explicit braces and semicolons

-- Step one: make a data type for propositional logic formulas
data PropFormula = Term Char Bool | Operator PropFormula PropFormula Char |
   Negation PropFormula deriving (Eq, Show)

-- Step two: recursive function to convert the formula to negation normal form
-- Cases needing conversion: negation on formulas, implies, nand, xor, equals
toNNF (Term x y) = Term x y
-- Disjunction
toNNF (Operator formOne formTwo 'A') =
   Operator (toNNF formOne) (toNNF formTwo) 'A'
-- Conjunction
toNNF (Operator formOne formTwo 'K') =
   Operator (toNNF formOne) (toNNF formTwo) 'K'
-- Implication
toNNF (Operator formOne formTwo 'C') =
   Operator (toNNF (Negation formOne)) (toNNF formTwo) 'A'
-- Nand
toNNF (Operator formOne formTwo 'D') =
   toNNF (Negation (Operator formOne formTwo 'K'))
-- Xor
toNNF (Operator formOne formTwo 'J') =
   Operator (Operator (toNNF formOne) (toNNF (Negation formTwo)) 'K')
      (Operator (toNNF (Negation formOne)) (toNNF formTwo) 'K') 'A'
-- Equivalence
toNNF (Operator formOne formTwo 'E') =
   toNNF (Operator (Operator formOne formTwo 'C')
      (Operator formTwo formOne 'C') 'K')

-- Negations of cases
toNNF (Negation (Term x y)) = Term x (not y)
toNNF (Negation (Negation formOne)) = toNNF formOne
toNNF (Negation (Operator formOne formTwo 'A')) =
   Operator (toNNF (Negation formOne)) (toNNF (Negation formTwo)) 'K'
toNNF (Negation (Operator formOne formTwo 'K')) =
   Operator (toNNF (Negation formOne)) (toNNF (Negation formTwo)) 'A'
toNNF (Negation (Operator formOne formTwo 'C')) =
   toNNF (Operator formOne (Negation formTwo) 'K')
toNNF (Negation (Operator formOne formTwo 'D')) =
   toNNF (Operator formOne formTwo 'K')
toNNF (Negation (Operator formOne formTwo 'J')) =
   toNNF (Negation (toNNF (Operator formOne formTwo 'J')))
toNNF (Negation (Operator formOne formTwo 'E')) =
   toNNF (Negation (toNNF (Operator formOne formTwo 'E')))

-- Step three: recursive function to convert negation to conjunctive norm form
toCNF (Term x y) = Term x y
toCNF (Operator (Term x y) (Term p q) 'A') =
   Operator (Term x y) (Term p q) 'A'
toCNF (Operator (Operator formOne formTwo 'K') formThree 'A') =
   Operator (toCNF (Operator formOne formThree 'A'))
      (toCNF (Operator formTwo formThree 'A')) 'K'
toCNF (Operator formThree (Operator formOne formTwo 'K') 'A') =
   Operator (toCNF (Operator formOne formThree 'A'))
      (toCNF (Operator formTwo formThree 'A')) 'K'

-- Step four: recursive function to check if a formula is a tautology
-- Base case: a single clause has "P" and "not P" for proposition P
-- Recursive case: the formula consists of multiple clauses
isTautology (Term x y) = "false" ++ x:[]

-- Step five: apply the above functions to string input and return output
isOperator x = (any (==x) ['A', 'C', 'D', 'E', 'J', 'K'])

toPropForm (x:[]) = Term x True
toPropForm (x:y:[]) = Term y False
toPropForm (x:y:z:[]) = Operator (Term z True) (Term y True) x
-- I don't think this is the right way to do the recursive case
toPropForm (x:y:z:l:[]) = if z == 'N'
   then Operator (Term l False) (Term y True) x
   else if y == 'N'
      then Operator (Term l True) (Term z False) x
      else Negation (Operator (Term l True) (Term z True) y)
-- This feels more correct for the recursive case, but I'm struggling with it
toPropForm (x:y:z:zs) = if x == 'N' then Negation (toPropForm y:z:zs)
   else if y == 'N' then if (or ((isOperator z):(z == 'N'):[]))
   

tautChecker = \ x -> unlines (map isTautology (map toCNF
   (map toNNF (map toPropForm (map reverse (lines x))))))
