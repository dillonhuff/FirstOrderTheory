module FirstOrderTheory.Utils(
  Name, Arity, Sort,
  sort) where

type Name = String
type Arity = Int

type Sort = String

sort :: String -> Sort
sort str = str
