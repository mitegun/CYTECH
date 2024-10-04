#Résolution d'un problème de PL à l'aide du module pulp

from pulp import *

prob = LpProblem("monprob", LpMaximize)
x1 = LpVariable("x1", lowBound=0, cat="Integer")
x2 = LpVariable("x2", lowBound=0, cat="Integer")

prob += 20*x1+30*x2

prob += 1*x1+2*x2 <= 100

prob += 2*x1+1*x2 <= 100

sol = prob.solve()

print(value(x1), value(x2), value(prob.objective))




