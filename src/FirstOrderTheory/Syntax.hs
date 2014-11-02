module FirstOrderTheory.Syntax(
  Literal,
  lit, nLit, predicate, func, var, intConst) where

import Data.List as L

import FirstOrderTheory.Utils

data Literal
  = Lit Predicate
  | Neg Predicate
    deriving (Eq, Ord)

lit = Lit
nLit = Neg

instance Show Literal where
  show (Lit p) = show p
  show (Neg p) = "~" ++ show p

data Predicate = Predicate Name [Term]
                 deriving (Eq, Ord)

predicate = Predicate

instance Show Predicate where
  show (Predicate name args) = name ++ "[" ++ (L.intercalate ", " $ L.map show args) ++ "]"

data Term
  = Function Name [Term]
  | Variable Name
  | Integer Int
    deriving (Eq, Ord)

func = Function
var = Variable
intConst = Integer

instance Show Term where
  show (Integer val) = show val
  show (Variable name) = name
  show (Function name args) = name ++ "(" ++ (L.intercalate ", " $ L.map show args) ++ ")"
