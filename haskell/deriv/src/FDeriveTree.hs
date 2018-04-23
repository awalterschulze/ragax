{-# LANGUAGE DeriveFunctor #-}

module FDeriveTree
    ( match
    , emptySet
    , empty
    , node
    , concat
    , zeroOrMore
    , or
    ) where

import Prelude hiding (concat, or)
import Data.Functor.Foldable (cata, unfix, Fix(..), para)
import qualified Data.Tree as Tree

type Pattern = Fix PatternF

data PatternF a = EmptySet
  | Empty
  | NodeExpr String a
  | Concat a a
  | ZeroOrMore a
  | Or a a
  deriving Functor

emptySet :: Pattern
emptySet = Fix EmptySet

empty :: Pattern
empty = Fix Empty

node :: String -> Pattern -> Pattern
node name child = Fix (NodeExpr name child)

concat :: Pattern -> Pattern -> Pattern
concat a b = Fix (Concat a b)

zeroOrMore :: Pattern -> Pattern
zeroOrMore a = Fix (ZeroOrMore a)

or :: Pattern -> Pattern -> Pattern
or a b = Fix (Or a b)

type Algebra f a = f a -> a

type NullableAlgebra = Algebra PatternF Bool

nullable' :: NullableAlgebra
nullable' EmptySet = False
nullable' Empty = True
nullable' NodeExpr{} = False
nullable' (Concat a b) = a && b
nullable' ZeroOrMore{} = True
nullable' (Or a b) = a || b

nullable :: Pattern -> Bool
nullable = cata nullable'

type RAlgebra f a = f (Fix f, a) -> a

type DeriveRAlgebra = RAlgebra PatternF Pattern

deriv' :: Tree.Tree String -> DeriveRAlgebra
deriv' _ Empty = emptySet
deriv' _ EmptySet = emptySet
deriv' (Tree.Node name children) (NodeExpr nameExpr (childPat, _)) = 
    if name == nameExpr && nullable (foldl deriv childPat children)
        then empty
        else emptySet
deriv' _ (Concat (r, dr) (s, ds)) =
  if nullable r
     then or (concat dr s) ds
     else concat dr s
deriv' _ (ZeroOrMore (dr, r)) =
  concat dr (zeroOrMore r)
deriv' _ (Or (_, dr) (_, ds)) =
  or dr ds

deriv :: Pattern -> Tree.Tree String -> Pattern
deriv pat tree = para (deriv' tree) pat

match :: Pattern -> Tree.Tree String -> Bool
match pat tree = nullable (deriv pat tree)
