module FirstOrderTheory.Utils(
  Name, Arity, Sort,
  sort) where

type Name = String
type Arity = Int

type Sort = String

-- |Make a new sort with the input name
sort :: String -> Sort
sort str = str
