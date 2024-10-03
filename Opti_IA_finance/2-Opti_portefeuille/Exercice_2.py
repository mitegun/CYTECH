import pandas as pd
import numpy as np
from scipy import stats
from cvxopt import matrix, solvers
import matplotlib.pyplot as plt

# Lecture des données
a = pd.read_csv('pourEc_Exam2.csv', usecols=[1, 2, 3])

# Calcul des retours
retour = a.pct_change().dropna()

# Calcul des moyennes
moyenne_a = retour.mean()
moyenne_g = stats.gmean(retour + 1) - 1

# Calcul de la matrice de covariance
cov = retour.cov()

# Convertir les données en matrices cvxopt
P = 2*matrix(cov.values)
q = matrix(np.zeros(cov.shape[0]))

rendement = [0.065, 0.07, 0.075, 0.08, 0.085, 0.09, 0.095, 0.10, 0.105]
risque = []

for r in rendement:
    # Contraintes Gx <= h (G est une matrice d'identité négative pour les contraintes de non-négativité)
    G = matrix(np.vstack((-np.eye(cov.shape[0]), -moyenne_g)))
    h = matrix(np.hstack((np.zeros(cov.shape[0]), -r)))

    # Contraintes A.T * x = b (A est un vecteur de 1 pour la contrainte de somme des poids égale à 1)
    A = matrix(1.0, (1, cov.shape[0]))
    b = matrix(1.0)

    # Résolution du problème d'optimisation
    sol = solvers.qp(P, q, G, h, A, b)

    # Extraction des poids optimaux
    weights = np.array(sol['x']).flatten()
   
    risk = 0.5 * np.dot(weights.T, np.dot(P, weights)) + np.dot(q.T, weights)
    risque.append(risk)

    print("Rendement =", r)
    print("Optimal Weights:", weights)
    print("Risque =", risk)

# Plot rendement vs risque
plt.plot(risque, rendement, marker='o')
plt.xlabel('Risque')
plt.ylabel('Rendement')
plt.title('Rendement en fonction du Risque')
plt.grid(True)
plt.show()