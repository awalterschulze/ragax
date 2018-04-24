{-# LANGUAGE DeriveFunctor #-}

module FDerive
    ( match
    , emptySet
    , emptyString
    , character
    , concat
    , zeroOrMore
    , or
    ) where

import Prelude hiding (concat, or)
import Data.Functor.Foldable (cata, unfix, Fix(..), para)

type Regex = Fix RegexF

data RegexF a = EmptySet
  | EmptyString
  | Character Char
  | Concat a a
  | ZeroOrMore a
  | Or a a
  deriving Functor

emptySet :: Regex
emptySet = Fix EmptySet

emptyString :: Regex
emptyString = Fix EmptyString

character :: Char -> Regex
character c = Fix (Character c)

concat :: Regex -> Regex -> Regex
concat a b = Fix (Concat a b)

zeroOrMore :: Regex -> Regex
zeroOrMore a = Fix (ZeroOrMore a)

or :: Regex -> Regex -> Regex
or a b = Fix (Or a b)

type Algebra f a = f a -> a

type NullableAlgebra = Algebra RegexF Bool

nullable' :: NullableAlgebra
nullable' EmptySet = False
nullable' EmptyString = True
nullable' Character{} = False
nullable' (Concat a b) = a && b
nullable' ZeroOrMore{} = True
nullable' (Or a b) = a || b

-- cata :: (RegexF Bool -> Bool) -> Regex -> Bool

nullable :: Regex -> Bool
nullable = cata nullable'

type RAlgebra f a = f (Fix f, a) -> a

type DeriveRAlgebra = RAlgebra RegexF Regex

deriv' :: Char -> DeriveRAlgebra
deriv' _ EmptyString = emptySet
deriv' _ EmptySet = emptySet
deriv' c (Character a) = if a == c 
    then emptyString else emptySet
deriv' c (Concat (r, dr) (s, ds)) =
  if nullable r
     then (dr `concat` s) `or` ds
     else dr `concat` s
deriv' _ (ZeroOrMore (dr, r)) =
  dr `concat` zeroOrMore r
deriv' _ (Or (_, dr) (_, ds)) =
  dr `or` ds

-- para :: (RegexF (Regex, Regex) -> Regex) -> Regex -> Regex

deriv :: Regex -> Char -> Regex
deriv expr c = para (deriv' c) expr

match :: Regex -> String -> Bool
match expr string = nullable (foldl deriv expr string)
