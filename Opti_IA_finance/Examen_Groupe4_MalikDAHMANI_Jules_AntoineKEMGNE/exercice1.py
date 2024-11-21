import pulp
from pulp import LpMaximize, LpProblem, value

#question 4


# Création du problème de maximisation
prob = LpProblem("Maximisation_du_benefice", LpMaximize)

# Définition des variables de décision avec leurs bornes
# Variables de décision
x = pulp.LpVariable('x', lowBound=0, cat='Integer')  # Nombre d'actions achetées
y = pulp.LpVariable('y', lowBound=-50, upBound=50, cat='Integer')  # Nombre d'options achetées/vendues
z = pulp.LpVariable('z', lowBound=0, cat='Integer')  # Nombre de bons achetés

# Ajout de la fonction objectif
prob += 4 * x + 0 * y + 10 * z, "Bénéfice total"

# Ajout des contraintes
prob += 20 * x + 1000 * y + 90 * z <= 20000, "Budget total"
prob += -y <= 50, "-y <= 50"
prob += y <= 50, "y <= 50"

# Résolution du problème
prob.solve()

# Affichage des résultats
if prob.status == 1:  # 1 corresponds to LpStatusOptimal
    print("Solution optimale trouvée:")
    print(f"Nombre d'actions achetées (x): {value(x):.2f}")
    print(f"Nombre d'options achetées/vendues (y): {value(y):.2f}")
    print(f"Nombre de bons achetés (z): {value(z):.2f}")
    print(f"Bénéfice attendu: {value(prob.objective):.2f} Euros")
else:
    print("Une solution optimale n'a pas pu être trouvée.")



#Question6

# Définir les paramètres pour chaque cas
scenarios = {
    "Cas 1": {"E_B_A": 10.8, "E_B_O": 625},
    "Cas 2": {"E_B_A": 0.8, "E_B_O": -300}
}

for case, params in scenarios.items():
    print(f"Résolution pour {case} :")
   
    # Créer un problème de maximisation
    prob = pulp.LpProblem("Maximisation_du_benefice", pulp.LpMaximize)

    # Variables de décision
    x = pulp.LpVariable('x', lowBound=0, cat='Integer')  # Nombre d'actions achetées
    y = pulp.LpVariable('y', lowBound=-50, upBound=50, cat='Integer')  # Nombre d'options achetées/vendues
    z = pulp.LpVariable('z', lowBound=0, cat='Integer')  # Nombre de bons achetés

    # Fonction objectif
    prob += params["E_B_A"] * x + params["E_B_O"] * y + 10 * z, "Bénéfice total"

    # Contraintes
    prob += 20 * x + 1000 * y + 90 * z <= 20000, "Budget total"
    prob += y >= -50, "y_min"
    prob += y <= 50, "y_max"

    # Résolution
    prob.solve()

    # Affichage des résultats
    if pulp.LpStatus[prob.status] == 'Optimal':
        print("Solution optimale trouvée:")
        print(f"Nombre d'actions achetées (x): {x.varValue}")
        print(f"Nombre d'options achetées/vendues (y): {y.varValue}")
        print(f"Nombre de bons achetés (z): {z.varValue}")
        print(f"Bénéfice attendu: {pulp.value(prob.objective):.2f} Euros")
    else:
        print("Une solution optimale n'a pas pu être trouvée.")
    print("-" * 30)
