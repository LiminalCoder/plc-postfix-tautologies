-- Style Tips:
-- Don't leave trailing white space in your code
-- Don't use tabs
-- Aim to keep lines under 80 characters
-- Use the CamelCase variable naming convention
-- Don't use explicit braces and semicolons

-- Step one: make a data type for propositional logic formulas
data propFormula = Term Char Bool | Operator Char propFormula |
   Operator Char propFormula propFormula

-- Step two: recursive function to convert the formula to negation normal form
-- Cases needing conversion: negation on formulas, implies, nand, xor, equals
toNNF (Term _ _) = (Term _ _)
toNNF (Operator 'N' (Term _ x)) = (Term _ not x)

-- Step three: recursive function to convert negation to conjunctive norm form

-- Step four: recursive function to check if a formula is a tautology
-- Base case: a single clause has "P" and "not P" for proposition P
-- Recursive case: the formula consists of multiple clauses