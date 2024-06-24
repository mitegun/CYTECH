{-# LANGUAGE KindSignatures #-}
{- |
  Module : Literal
  Description : A module representing a literal element in a normal form
  Maintainer  : ???
-}
module Literal(Literal(..), fromBool, fromPositive, fromNegative, neg, toFormula) where

import Data.Kind

import Formula (Formula(..))

-- | A literal element may be :
-- | * a boolean constant
-- | * a logical variable or the negation of it
data Literal
  = BoolConst Bool
  | LogicalVar String
  | Not Literal 
  deriving (Eq, Ord)

instance Show Literal where
  show (BoolConst True) = "True"
  show (BoolConst False) = "False"
  show (LogicalVar var) = var
  show (Literal.Not literal) = "¬" ++ show literal

-- | Convert boolean value to constant literal
fromBool :: Bool -> Literal
fromBool True = BoolConst True
fromBool False = BoolConst False

-- | Convert logical variable to a positive literal
fromPositive :: String -> Literal
fromPositive var = LogicalVar var

-- | Convert logical variable to a negative literal
fromNegative :: String -> Literal
fromNegative var = Literal.Not (LogicalVar var)

-- | Negation operation
neg :: Literal -> Literal
neg (BoolConst b) = BoolConst (not b)
neg (LogicalVar var) = Literal.Not (LogicalVar var)
neg (Literal.Not l) = l

-- | Convert a literal to the corresponding logical formula
toFormula :: Literal -> Formula
toFormula (BoolConst True) = Vrai
toFormula (BoolConst False) = Faux
toFormula (LogicalVar var) = Var var
toFormula (Literal.Not (Literal.Not literal)) = toFormula literal 
toFormula (Literal.Not literal) = Formula.Not (toFormula literal)

