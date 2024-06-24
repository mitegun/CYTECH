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
    -- Création de quelques formules
    let formula1 = Formula.Var "A" `Formula.conj` (Formula.Var "B" `Formula.disj` Formula.Var "C")
    let formula2 = Formula.neg (Formula.Var "A") `Formula.implies` Formula.Var "D"

    -- Affichage des littéraux
    putStrLn "Littéraux :"
    putStrLn $ "Littéral booléen : " ++ show boolLiteral
    putStrLn $ "Littéral positif : " ++ show posLiteral
    putStrLn $ "Littéral négatif : " ++ show negLiteral

    -- Affichage des formules
    putStrLn "\nFormules :"
    putStrLn $ "Formule 1 : " ++ show formula1
    putStrLn $ "Formule 2 : " ++ show formula2

    -- Évaluation des formules en utilisant les littéraux
    let environment1 = Map.fromList [("A", True), ("B", False), ("C", True), ("D", False)]
    let environment2 = Map.fromList [("A", False), ("B", True), ("C", False), ("D", True)]

    putStrLn "\nÉvaluation des formules en utilisant les littéraux :"
    putStrLn $ "Formule 1 dans l'environnement 1 : " ++ show (Formula.evaluate environment1 formula1)
    putStrLn $ "Formule 2 dans l'environnement 2 : " ++ show (Formula.evaluate environment2 formula2)

    -- Vérification d'équivalence en utilisant les littéraux
    putStrLn "\nVérification d'équivalence en utilisant les littéraux :"
    putStrLn $ "Formule 1 <=> Formule 2 : " ++ show (formula1 Formula.<=> formula2)

    -- Vérification de tautologie en utilisant les littéraux
    putStrLn "\nVérification de tautologie en utilisant les littéraux :"
    putStrLn $ "La Formule 1 est-elle une tautologie ? " ++ show (Formula.tautology formula1)
    
    -- Crée une formule logique
    let formula = Var "A" `conj` (Var "B" `disj` Var "C")

    -- Convertit la formule en CNF
    let cnf = fromFormula formula

    -- Affiche des informations
    putStrLn $ "Formule originale : " ++ show formula
    putStrLn $ "Représentation CNF : " ++ show cnf
    putStrLn $ "Nombre de littéraux de la CNF : " ++ show (NormalForm.size cnf)

    -- Convertit CNF en Formule pour simplification
    let formulaFromCNF = NormalForm.toFormula cnf
    putStrLn $ "Converti en Formule : " ++ show formulaFromCNF

    -- Simplification de la formule convertie de CNF
    let simplifiedFormula = simplify formulaFromCNF
    putStrLn $ "Formule simplifiée : " ++ show simplifiedFormula

    -- Conversion de la formule simplifiée en CNF pour affichage
    let simplifiedCNF = fromFormula simplifiedFormula
    putStrLn $ "CNF de la formule simplifiée : " ++ show simplifiedCNF

    -- Applique la règle de Robinson à la CNF
    let cnfAfterRobinson = robinson cnf
    putStrLn $ "Après application de la règle de Robinson : " ++ show cnfAfterRobinson
-- Création de quelques formules
    let formula1 = Formula.Var "A" `Formula.And` (Formula.Var "B" `Formula.Or` Formula.Var "C")
    let formula2 = Formula.Not (Formula.Var "A") `Formula.Or` Formula.Var "D"

    -- Conversion des formules en CNF
    let cnf1 = NormalForm.fromFormula formula1
    let cnf2 = NormalForm.fromFormula formula2

    -- Affichage des formules et de leur CNF correspondant
    putStrLn "Formules et leurs CNF correspondants:"
    putStrLn $ "Formule 1: " ++ show formula1
    putStrLn $ "CNF de Formule 1: " ++ show cnf1
    putStrLn $ "Formule 2: " ++ show formula2
    putStrLn $ "CNF de Formule 2: " ++ show cnf2

    -- Affichage de la taille des CNF
    putStrLn $ "Nombre de littéraux de CNF 1: " ++ show (NormalForm.size cnf1)
    putStrLn $ "Nombre de littéraux de CNF 2: " ++ show (NormalForm.size cnf2)

    -- Conversion des CNF en formules et affichage
    putStrLn $ "Formule convertie à partir de CNF 1: " ++ show (NormalForm.toFormula cnf1)
    putStrLn $ "Formule convertie à partir de CNF 2: " ++ show (NormalForm.toFormula cnf2)

    -- Application de la règle de Robinson
    let cnf1Robinson = NormalForm.robinson cnf1
    let cnf2Robinson = NormalForm.robinson cnf2
    putStrLn $ "CNF 1 après application de la règle de Robinson: " ++ show cnf1Robinson
    putStrLn $ "CNF 2 après application de la règle de Robinson: " ++ show cnf2Robinson

    -- Ajout de tests de vérification de tautologie pour les différentes formules
    putStrLn "\nVérification de la tautologie pour les formules originales et converties:"
    putStrLn $ "La formule originale est-elle une tautologie? " ++ show (Formula.tautology formula)
    putStrLn $ "La formule 1 est-elle une tautologie? " ++ show (Formula.tautology formula1)
    putStrLn $ "La formule 2 est-elle une tautologie? " ++ show (Formula.tautology formula2)
    putStrLn $ "La CNF convertie de la formule 1 est-elle une tautologie? " ++ show (Formula.tautology (NormalForm.toFormula cnf1))
    putStrLn $ "La CNF convertie de la formule 2 est-elle une tautologie? " ++ show (Formula.tautology (NormalForm.toFormula cnf2))
    putStrLn $ "La CNF après application de la règle de Robinson pour la formule 1 est-elle une tautologie? " ++ show (Formula.tautology (NormalForm.toFormula cnf1Robinson))
    putStrLn $ "La CNF après application de la règle de Robinson pour la formule 2 est-elle une tautologie? " ++ show (Formula.tautology (NormalForm.toFormula cnf2Robinson))
    
    -- Création des nouvelles formules pour le test de Robinson
    let formula3 = (Formula.Var "P" `Formula.Or` Formula.Var "Q") `Formula.And` (Formula.Not (Formula.Var "P") `Formula.Or` Formula.Var "R")
    let formula4 = (Formula.Var "D" `Formula.Or` Formula.Var "E") `Formula.And` (Formula.Not (Formula.Var "D") `Formula.Or` Formula.Var "F")

    -- Conversion des nouvelles formules en CNF
    let cnf3 = NormalForm.fromFormula formula3
    let cnf4 = NormalForm.fromFormula formula4

    -- Affichage des nouvelles formules et de leur CNF correspondant
    putStrLn "\nNouvelles formules pour le test de Robinson:"
    putStrLn $ "Formule 3: " ++ show formula3
    putStrLn $ "CNF de Formule 3: " ++ show cnf3
    putStrLn $ "Formule 4: " ++ show formula4
    putStrLn $ "CNF de Formule 4: " ++ show cnf4

    -- Application de la règle de Robinson sur les nouvelles CNF
    let cnf3Robinson = NormalForm.robinson cnf3
    let cnf4Robinson = NormalForm.robinson cnf4
    putStrLn $ "CNF 3 après application de la règle de Robinson: " ++ show cnf3Robinson
    putStrLn $ "CNF 4 après application de la règle de Robinson: " ++ show cnf4Robinson

    -- Vérification de la tautologie pour les nouvelles CNF après Robinson
    putStrLn "\nVérification de la tautologie pour les CNF après Robinson:"
    putStrLn $ "La CNF 3 après Robinson est-elle une tautologie? " ++ show (Formula.tautology (NormalForm.toFormula cnf3Robinson))
    putStrLn $ "La CNF 4 après Robinson est-elle une tautologie? " ++ show (Formula.tautology (NormalForm.toFormula cnf4Robinson))
