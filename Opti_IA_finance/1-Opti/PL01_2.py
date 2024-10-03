#Résolution d'un problème de PL à l'aide du module pulp

from pulp import *

#Créer le problème
prob = LpProblem("monprob", LpMaximize)

#Créer les variables
x1 = LpVariable("x1", lowBound=0)
x2 = LpVariable("x2", lowBound=0)

#La fonction à optimiser
prob += 20*x1+30*x2

#Les contraintes

prob += 1*x1+2*x2 <= 100

prob += 2*x1+1*x2 <= 100

#La résolution

sol = prob.solve()

#Affichage de la solution

print('x1=',value(x1), 'x2=', value(x2), 'Max=',value(prob.objective))




