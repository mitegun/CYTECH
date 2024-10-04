from itertools import combinations
from pulp import LpMaximize, LpProblem, LpVariable, lpSum, value

#Version pas opti
a=[1,2,3,4]
lInvest=[6700,10000,5500,3400]
lRetour=[8000,11000,6000,4000]
budget_max=19000
lTuples=[]

for i in range(len(a)+1):
    lTuples.extend(list(combinations(a,i)))
cond=0
retour=0
for tup in lTuples:
    for i in tup:
        cond+=lInvest[i-1]
        retour+=lRetour[i-1]
    if cond<=budget_max:
        print(tup,cond,retour)
    cond=0
    retour=0

#Version Opti 
prob = LpProblem("Optimisation", LpMaximize)
x = LpVariable.dicts("x", range(len(a)), cat='Binary')
prob += lpSum([lRetour[i] * x[i] for i in range(len(a))]), "Retour"
prob += lpSum([lInvest[i] * x[i] for i in range(len(a))]) <= budget_max, "Contrainte"
prob.solve()
for i in range(len(a)):
    if x[i].varValue == 1:
        print(f"Invest {i+1}: Cout = {lInvest[i]}, Retour = {lRetour[i]}")
print("Total Cout:", value(lpSum([lInvest[i] * x[i] for i in range(len(a))])))
print("Total Retour:", value(lpSum([lRetour[i] * x[i] for i in range(len(a))])))

#pas plus de deux
prob = LpProblem("Optimisation", LpMaximize)
x = LpVariable.dicts("x", range(len(a)), cat='Binary')
prob += lpSum([lRetour[i] * x[i] for i in range(len(a))]), "Retour"
prob += lpSum([lInvest[i] * x[i] for i in range(len(a))]) <= budget_max, "Contrainte"
prob += lpSum([ x[i] for i in range(len(a))]) <= 2, "Contrainte deux projts max"
prob.solve()
for i in range(len(a)):
    if x[i].varValue == 1:
        print(f"Invest {i+1}: Cout = {lInvest[i]}, Retour = {lRetour[i]}")
print("Total Cout:", value(lpSum([lInvest[i] * x[i] for i in range(len(a))])))
print("Total Retour:", value(lpSum([lRetour[i] * x[i] for i in range(len(a))])))

#si 1 pas ensemble 3 et si 2 alors 4 et pas plus de deux
prob = LpProblem("Optimisation", LpMaximize)
x = LpVariable.dicts("x", range(len(a)), cat='Binary')
prob += lpSum([lRetour[i] * x[i] for i in range(len(a))]), "Retour"
prob += lpSum([lInvest[i] * x[i] for i in range(len(a))]) <= budget_max, "Contrainte"
prob += x[0]+x[2]<=1, "Contrainte 1"
prob += x[1]-x[3]<=0, "Contrainte 2"
prob += lpSum([ x[i] for i in range(len(a))]) <= 2, "Contrainte deux projts max"
prob.solve()
for i in range(len(a)):
    if x[i].varValue == 1:
        print(f"Invest {i+1}: Cout = {lInvest[i]}, Retour = {lRetour[i]}")
print("Total Cout:", value(lpSum([lInvest[i] * x[i] for i in range(len(a))])))
print("Total Retour:", value(lpSum([lRetour[i] * x[i] for i in range(len(a))])))

