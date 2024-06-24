module Main where

import Formula
import Literal
import qualified Data.Map as Map
import NormalForm


main :: IO ()
main = do
    -- Création de quelques littéraux
    let boolLiteral = Literal.fromBool True
    let posLiteral = Literal.fromPositive "A"
    let negLiteral = Literal.fromNegative "B"
    let lit1 = Literal.BoolConst True
    let lit2 = Literal.LogicalVar "A"
    let lit3 = Literal.Not (Literal.LogicalVar "B")
    let lit = Literal.LogicalVar "P"
    let doubleNegation = Literal.Not (Literal.Not lit)

    putStrLn "Test de la double negation:"
    putStrLn $ "Literal: " ++ show lit
    putStrLn $ "Test de la double Négation: " ++ show doubleNegation
    putStrLn $ "Conversion en Formule " ++ show (Literal.toFormula doubleNegation)
    putStrLn "Test de toFormula"
    putStrLn $ "Literal: " ++ show lit1
    putStrLn $ "Literal: " ++ show lit2
    putStrLn $ "Literal: " ++ show lit3           
    putStrLn $ "toFormula " ++ show lit1 ++ ": " ++ show (Literal.toFormula lit1)
    putStrLn $ "toFormula " ++ show lit2 ++ ": " ++ show (Literal.toFormula lit2)
    putStrLn $ "toFormula " ++ show lit3 ++ ": " ++ show (Literal.toFormula lit3)
