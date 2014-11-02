module FirstOrderTheory.Syntax(
  Literal, Atom, Term,
  atomArgs, predicateName, isFunctionWithName,
  isNeg, getAtom, varName, intVal, funcArgs,
  lit, nLit, atom, func, var, intConst) where

import Data.List as L

import FirstOrderTheory.Utils

data Literal
  = Lit Atom
  | Neg Atom
    deriving (Eq, Ord)

lit = Lit
nLit = Neg

-- |Returns false if the literal is an atomic formula and true if the
-- literal is the negation of an atomic formula
isNeg :: Literal -> Bool
isNeg (Neg _) = True
isNeg _ = False

-- |Returns the atomic formula of the given literal
getAtom :: Literal -> Atom
getAtom (Neg a) = a
getAtom (Lit a) = a

instance Show Literal where
  show (Lit p) = show p
  show (Neg p) = "~" ++ show p

data Atom = Atom Name [Term]
                 deriving (Eq, Ord)

-- |Returns a new atom
atom = Atom

-- |Return the terms that are arguments to the predicate of the input atom
atomArgs (Atom _ args) = args

-- |Return the name of the predicate of the input atom
predicateName (Atom n _) = n

instance Show Atom where
  show (Atom name args) = name ++ "[" ++ (L.intercalate ", " $ L.map show args) ++ "]"

data Term
  = Function Name [Term]
  | Variable Name
  | Integer Int
    deriving (Eq, Ord)

func = Function
var = Variable
intConst = Integer

-- |Returns True if the input term is a function with the give name,
-- and false otherwise
isFunctionWithName n (Function m _) = n == m
isFunctionWithName _ _ = False

-- |Return the name of the input variable,
-- throws error if input term is not a variable
varName (Variable n) = n

-- |Returns the input function's argument list,
-- throws error if input term is not a function
funcArgs (Function _ args) = args

-- |Returns the integer value of an Integer constant or an error
-- if the input is not an integer
intVal (Integer i) = i

instance Show Term where
  show (Integer val) = show val
  show (Variable name) = name
  show (Function name args) = name ++ "(" ++ (L.intercalate ", " $ L.map show args) ++ ")"
