module FirstOrderTheory.Theory(
  FirstOrderTheory(..),
  predicateDecl,
  functionDecl) where

import Data.Set as S

import FirstOrderTheory.Syntax
import FirstOrderTheory.Utils

data PredicateDecl = PredicateDecl Name Arity [Sort]
                     deriving (Eq, Ord)

predicateDecl = PredicateDecl

data FunctionDecl = FunctionDecl Name Arity [Sort] Sort
                    deriving (Eq, Ord)

functionDecl = FunctionDecl

class FirstOrderTheory t where
  theoryName :: t -> String
  sorts :: t -> Set Sort
  predicates :: t -> Set PredicateDecl
  functions :: t -> Set FunctionDecl
  decideSat :: t -> Set Literal -> (Bool, Set Literal)
