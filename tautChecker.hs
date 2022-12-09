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
toNNF (Negation (Term x y)) = Term x (not y)
toNNF (Negation (Negation formOne)) = toNNF formOne
toNNF (Negation (Operator formOne formTwo 'A')) =
   Operator (toNNF (Negation formOne)) (toNNF (Negation formTwo)) 'K'
toNNF (Negation (Operator formOne formTwo 'K')) =
   Operator (toNNF (Negation formOne)) (toNNF (Negation formTwo)) 'A'
toNNF (Operator formOne formTwo 'C') =
   Operator (toNNF (Negation formOne)) (toNNF formTwo) 'A'
toNNF (Operator formOne formTwo 'D') =
   toNNF (Negation (Operator formOne formTwo 'K'))
toNNF (Operator formOne formTwo 'J') =
   Operator (Operator (toNNF formOne) (toNNF (Negation formTwo)) 'K')
      (Operator (toNNF (Negation formOne)) (toNNF formTwo) 'K') 'A'
toNNF (Operator formOne formTwo 'E') =


-- Step three: recursive function to convert negation to conjunctive norm form

-- Step four: recursive function to check if a formula is a tautology
-- Base case: a single clause has "P" and "not P" for proposition P
-- Recursive case: the formula consists of multiple clauses