module FirstOrderTheory.Syntax(Literal) where

import FirstOrderTheory.Utils

data Literal
  = Lit Predicate
  | Neg Predicate

data Predicate = Predicate Name [Term]

data Term
  = Function Name [Term]
  | Variable Name
