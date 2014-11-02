module FirstOrderTheory.Theory(
  FirstOrderTheory(..),) where

import Data.Set as S

import FirstOrderTheory.Syntax
import FirstOrderTheory.Utils

data PredicateDecl = PredicateDecl Name Arity [Sort]

predicateDecl = PredicateDecl

data FunctionDecl = FunctionDecl Name Arity [Sort] Sort

functionDecl = FunctionDecl

class FirstOrderTheory t where
  sorts :: t -> Set Sort
  predicates :: t -> Set PredicateDecl
  functions :: t -> Set FunctionDecl
  decideSat :: t -> Set Literal -> Either Bool (Set Literal)
