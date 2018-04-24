module Derive
    ( Regex(..), match
    ) where

data Regex = EmptySet
  | EmptyString
  | Character Char
  | Concat Regex Regex
  | ZeroOrMore Regex
  | Or Regex Regex

nullable :: Regex -> Bool
nullable EmptySet = False
nullable EmptyString = True
nullable Character{} = False
nullable (Concat a b) = nullable a && nullable b
nullable ZeroOrMore{} = True
nullable (Or a b) = nullable a || nullable b

deriv :: Regex -> Char -> Regex
deriv EmptyString _ = EmptySet
deriv EmptySet _ = EmptySet
deriv (Character a) c = if a == c 
  then EmptyString else EmptySet
deriv (Concat r s) c = if nullable r
  then (deriv r c `Concat` s) `Or` deriv s c
  else deriv r c `Concat` s
deriv (ZeroOrMore r) c =
  deriv r c `Concat` ZeroOrMore r
deriv (Or r s) c =
  deriv r c `Or` deriv s c

match :: Regex -> String -> Bool
match expr string = nullable (foldl deriv expr string)
