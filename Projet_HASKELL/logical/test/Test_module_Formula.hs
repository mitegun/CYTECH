import Formula
import Data.Map (Map)
import qualified Data.Map as Map

main :: IO ()
main = do
    -- Création de formules simples
    let varA = fromString "A"
    let varB = fromString "B"
    let constTrue = fromBool True

    -- Opérations de base
    let negation = neg varA
    let conjunction = conj varA varB
    let disjunction = disj varA constTrue

    -- Affichage des formules
    putStrLn $ "Négation de A: " ++ show negation
    putStrLn $ "Conjonction de A et B: " ++ show conjunction
    putStrLn $ "Disjonction de A et Vrai: " ++ show disjunction

    -- Création d'un environnement et évaluation
    let env = Map.fromList [("A", True), ("B", False)]
    putStrLn $ "Évaluation de la conjonction dans l'environnement: " ++ show (evaluate env conjunction)

    -- Création de formules complexes
    let formula1 = implies varA varB
    let formula2 = disj varA (neg varA)

    -- Simplification des formules
    putStrLn $ "Formule 1 avant simplification: " ++ show formula1
    putStrLn $ "Formule 1 après simplification: " ++ show (simplify formula1)

    putStrLn $ "Formule 2 avant simplification: " ++ show formula2
    putStrLn $ "Formule 2 après simplification: " ++ show (simplify formula2)

    -- Vérification de tautologie
    putStrLn $ "La formule 2 est-elle une tautologie? " ++ show (tautology formula2)

    -- Vérification de l'équivalence logique
    putStrLn $ "Formule 1 et Formule 2 sont-elles équivalentes? " ++ show (formula1 <=> formula2)

