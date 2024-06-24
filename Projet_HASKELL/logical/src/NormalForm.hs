{-# LANGUAGE KindSignatures #-}

module NormalForm (CNF(..), size, toFormula, fromFormula, robinson) where

import Data.Set (Set)
import qualified Data.Set as Set
import Formula (Formula(..))
import qualified Formula
import Literal (Literal(..))
import qualified Literal


-- | A custom intercalate function to join strings with a separator
customInter :: String -> [String] -> String
customInter sep = foldr (\a b -> a ++ if null b then b else sep ++ b) ""

-- | A conjunctive normal form
newtype CNF = CNF (Set (Set Literal)) deriving Eq

instance Show CNF where
  show (CNF clauses) = customInter " ∧ " (map showClause (Set.toList clauses))
    where
      showClause clause = "(" ++ customInter " ∨ " (map show (Set.toList clause)) ++ ")"


-- | Size (number of literals)
-- | Size (number of clauses)
size :: CNF -> Int
size (CNF clauses) = Set.size clauses

-- | Convert normal form to logical formula
toFormula :: CNF -> Formula
toFormula (CNF clauses) = foldr Formula.And Formula.Vrai (map clauseToFormula (Set.toList clauses))
  where
    clauseToFormula clause = foldr Formula.Or Formula.Faux (map literalToFormula (Set.toList clause))
    literalToFormula :: Literal -> Formula
    literalToFormula (Literal.BoolConst True) = Formula.Vrai
    literalToFormula (Literal.BoolConst False) = Formula.Faux
    literalToFormula (Literal.LogicalVar var) = Formula.Var var
    literalToFormula (Literal.Not l) = Formula.Not (literalToFormula l)

-- | Convert logical formula to normal form
-- Note: This is a placeholder implementation and may not correctly convert all formulas to CNF.
-- Convert logical formula to normal form (CNF)
fromFormula :: Formula -> CNF
fromFormula formula = CNF (convertToCNF (Formula.simplify formula))
  where
    convertToCNF :: Formula -> Set (Set Literal)
    convertToCNF Vrai = Set.singleton (Set.singleton (Literal.BoolConst True))
    convertToCNF Faux = Set.singleton (Set.singleton (Literal.BoolConst False))
    convertToCNF (Var x) = Set.singleton (Set.singleton (Literal.LogicalVar x))
    convertToCNF (Formula.Not f) = Set.map (Set.map Literal.neg) (convertToCNF f)
    convertToCNF (And f1 f2) = Set.union (convertToCNF f1) (convertToCNF f2)
    convertToCNF (Or f1 f2) = distributeOr (convertToCNF f1) (convertToCNF f2)

    distributeOr :: Set (Set Literal) -> Set (Set Literal) -> Set (Set Literal)
    distributeOr clauses1 clauses2 = 
        Set.fromList [Set.union c1 c2 | c1 <- Set.toList clauses1, c2 <- Set.toList clauses2]



-- | Apply ROBINSON's rule on clauses
-- Note: This is a placeholder implementation.
-- Apply Robinson's rule on CNF
robinson :: CNF -> CNF
robinson (CNF clauses) = CNF (applyRobinson clauses)
  where
    applyRobinson :: Set (Set Literal) -> Set (Set Literal)
    applyRobinson cls = 
      let pairs = [(c1, c2) | c1 <- Set.toList cls, c2 <- Set.toList cls, c1 /= c2]
      in Set.fromList [c | (c1, c2) <- pairs, isResolvable c1 c2, let c = resolve c1 c2, not (isTautology c)]

    isResolvable :: Set Literal -> Set Literal -> Bool
    isResolvable c1 c2 = not . Set.null $ Set.intersection (Set.map Literal.neg c1) c2

    resolve :: Set Literal -> Set Literal -> Set Literal
    resolve c1 c2 = 
       let
               literals_to_remove = Set.intersection (Set.map Literal.neg c1) c2
               c1' = foldr Set.delete c1 (Set.map Literal.neg literals_to_remove)
               c2' = foldr Set.delete c2 literals_to_remove
       in
               Set.union c1' c2'


    isTautology :: Set Literal -> Bool
    isTautology clause = any (\l -> Set.member (Literal.neg l) clause) clause



-- Additional helper functions (like intercalate) can be added here if necessary.

