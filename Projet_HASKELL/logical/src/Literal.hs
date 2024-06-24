{-# LANGUAGE KindSignatures #-}

{- |
  Module : Literal
  Description : A module representing a literal element in a normal form
  Maintainer  : ???
-}
module Literal(Literal(..), fromBool, fromPositive, fromNegative, neg, toFormula) where

-- A literal element may be:
-- * a boolean constant
-- * a logical variable or the negation of it
data Literal
  = BoolConst Bool
  | LogicalVar String
  | Not Literal
  deriving (Eq, Ord)

instance Show Literal where
  show (BoolConst True) = "True"
  show (BoolConst False) = "False"
  show (LogicalVar var) = var
  show (Not literal) = "¬" ++ show literal

-- | Convert boolean value to constant literal
fromBool :: Bool -> Literal
fromBool True = BoolConst True
fromBool False = BoolConst False

-- | Convert logical variable to a positive literal
fromPositive :: String -> Literal
fromPositive var = LogicalVar var

-- | Convert logical variable to a negative literal
fromNegative :: String -> Literal
fromNegative var = Not (LogicalVar var)

-- | Negation operation
neg :: Literal -> Literal
neg (BoolConst b) = BoolConst (not b)
neg (LogicalVar var) = Not (LogicalVar var)
neg (Not l) = l

-- Placeholder for the Formula type (toFormula function)
data Formula = Atom String | NotFormula Formula deriving (Eq, Show)

-- | Convert a literal to the corresponding logical formula
toFormula :: Literal -> Formula
toFormula (BoolConst True) = Atom "true"
toFormula (BoolConst False) = Atom "false"
toFormula (LogicalVar var) = Atom var
toFormula (Not l) = NotFormula (toFormula l)
