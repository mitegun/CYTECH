{-# LANGUAGE KindSignatures #-}
{- |
  Module : Formula
  Description : A module representing logical predicates with logical variables.
  Maintainer  : ???
-}

module Formula
  ( Formula(..)
  , fromBool
  , fromString
  , neg
  , conj
  , disj
  , implies
  , isLiteral
  , has
  , size
  , variables
  , Environment
  , evaluate
  , (<=>)
  , tautology
  , simplify
  ) where

import Data.Kind

import Data.Set (Set)
import qualified Data.Set as Set hiding (Set)
import Data.Map (Map)
import qualified Data.Map as Map hiding (Map)


-- Formula Data Type
data Formula
  = Var String  
  | Vrai      
  | Faux      
  | Not Formula              
  | And Formula Formula  
  | Or Formula Formula  
  deriving Eq
  -- !!! Implementation MUST derive typeclass 'Eq' so the previous line of code must be uncommented

-- CONSTRUCTORS

-- | Convert a boolean value into a constant logical formula
fromBool :: Bool -> Formula
fromBool b = if b then Vrai else Faux

-- | Convert a variable name into a formula with only the corresponding logical variable
fromString :: String -> Formula
fromString s = Var s

-- | Negation operation
neg :: Formula -> Formula
neg = Not

-- | Conjunction operation (logical "and")
conj :: Formula -> Formula -> Formula
conj = And

-- | Disjunction operation (logical "or")
disj :: Formula -> Formula -> Formula
disj = Or

-- | Implies operation
implies :: Formula -> Formula -> Formula
implies p q = Or (Not p) q

-- | Is the formula literal ?
isLiteral :: Formula -> Bool
isLiteral (Var _) = True
isLiteral Vrai = True
isLiteral Faux = True
isLiteral _ = False

-- Fonction équivalente à fromMaybe
defaultFromMaybe :: a -> Maybe a -> a
defaultFromMaybe defVal maybeVal = case maybeVal of
    Just val -> val
    Nothing -> defVal


-- Fonction auxiliaire pour compter les occurrences d'une variable
countOccurrences :: Formula -> String -> Int
countOccurrences (Var s) name = if s == name then 1 else 0
countOccurrences Vrai _ = 0
countOccurrences Faux _ = 0
countOccurrences (Not f) name = countOccurrences f name
countOccurrences (And f1 f2) name = countOccurrences f1 name + countOccurrences f2 name
countOccurrences (Or f1 f2) name = countOccurrences f1 name + countOccurrences f2 name

-- | Search for logical variable in formula
has :: Formula -> String -> Bool
has formula name = countOccurrences formula name > 0


-- | Size (number of operators)
size :: Formula -> Int
size Vrai = 1
size Faux = 1
size (Var _) = 1
size (Not f) = 1 + size f
size (And f1 f2) = 1 + size f1 + size f2
size (Or f1 f2) = 1 + size f1 + size f2

instance Show Formula where
  show (Var b) = "Var " ++ show b
  show Vrai = "Vrai"
  show Faux = "Faux"
  show (Not f) = "(Not " ++ show f ++ ")"
  show (And f1 f2) = "(And " ++ show f1 ++ " " ++ show f2 ++ ")"
  show (Or f1 f2) = "(Or " ++ show f1 ++ " " ++ show f2 ++ ")"

-- | Environment associating logical variables to logical values
type Environment = Map String Bool

-- | Evaluation (if possible) of formula in a given environment
evaluate :: Environment -> Formula -> Maybe Bool
evaluate env (Var s) = Map.lookup s env
evaluate _ Vrai = Just True
evaluate _ Faux = Just False
evaluate env (Not f) = fmap not (evaluate env f)
evaluate env (And f1 f2) =
    case (evaluate env f1, evaluate env f2) of
        (Just v1, Just v2) -> Just (v1 && v2)
        _ -> Nothing
evaluate env (Or f1 f2) =
    case (evaluate env f1, evaluate env f2) of
        (Just v1, Just v2) -> Just (v1 || v2)
        _ -> Nothing

-- | Retrieve set of all variables occuring in formula
variables :: Formula -> Set String
variables (Var s) = Set.singleton s
variables Vrai = Set.empty
variables Faux = Set.empty
variables (Not f) = variables f
variables (And f1 f2) = Set.union (variables f1) (variables f2)
variables (Or f1 f2) = Set.union (variables f1) (variables f2)

-- | Logical equivalence on formulae
(<=>) :: Formula -> Formula -> Bool
formula1 <=> formula2 =
    let sf1 = simplify formula1
        sf2 = simplify formula2
        vars = Set.toList $ Set.union (variables sf1) (variables sf2)
    in all (\env -> evaluate env sf1 == evaluate env sf2) (generateEnvironments vars)

-- | Is the formula a tautology ?
tautology :: Formula -> Bool
tautology formula =
    let sf = simplify formula
        vars = Set.toList $ variables sf
    in all (\env -> defaultFromMaybe False (evaluate env sf)) (generateEnvironments vars)


-- Fonction auxiliaire pour générer des environnements
generateEnvironments :: [String] -> [Environment]
generateEnvironments [] = [Map.empty]
generateEnvironments (v:vs) =
    let envs = generateEnvironments vs
    in [Map.insert v val env | env <- envs, val <- [True, False]]

-- | Attempts to simplify the proposition
simplify :: Formula -> Formula
simplify (Or p (Not q)) | p == q = Vrai  -- Tautologie: A ∨ ¬A
simplify (Or (Not p) q) | p == q = Vrai  -- Tautologie: ¬A ∨ A
simplify (And p (Not q)) | p == q = Faux -- Contradiction: A ∧ ¬A
simplify (And (Not p) q) | p == q = Faux -- Contradiction: ¬A ∧ A

simplify (Not (Not p)) = simplify p
simplify (And p Faux) = Faux
simplify (And Faux p) = Faux
simplify (Or p Vrai) = Vrai
simplify (Or Vrai p) = Vrai
simplify (And Vrai p) = simplify p
simplify (And p Vrai) = simplify p
simplify (Or Faux p) = simplify p
simplify (Or p Faux) = simplify p
simplify (Not Vrai) = Faux
simplify (Not Faux) = Vrai

simplify (And p1 p2) = 
  let sp1 = simplify p1
      sp2 = simplify p2
  in if sp1 == sp2 then sp1
     else if sp1 == Faux || sp2 == Faux then Faux
     else if sp1 == Vrai then sp2
     else if sp2 == Vrai then sp1
     else And sp1 sp2

simplify (Or p1 p2) = 
  let sp1 = simplify p1
      sp2 = simplify p2
  in if sp1 == sp2 then sp1
     else if sp1 == Vrai || sp2 == Vrai then Vrai
     else if sp1 == Faux then sp2
     else if sp2 == Faux then sp1
     else Or sp1 sp2

simplify p = p -- Cas de base sans simplification

