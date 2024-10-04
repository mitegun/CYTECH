from pulp import LpMaximize, LpProblem, LpVariable, lpSum, value

interet1=0.01
interet2=0.02
taux=0.003
a=[1,2,3,4,5,6]
b=[1,2,3]
prob = LpProblem("Optimisation", LpMaximize)
x = LpVariable.dicts("x", range(len(a)), lowBound=0)
y = LpVariable.dicts("y", range(len(b)), lowBound=0)
z = LpVariable.dicts("z", range(len(a)), lowBound=0)

prob+= z[5]
prob += lpSum([x[i] for i in range(len(a))]) <= 100, "Contrainte"
prob += x[0]+y[0]-z[0]==150, "contrainte janvier"
prob += x[1]+y[1]-1.01*x[0] + 1.003*z[0]-z[1]==100, "contrainte février"
prob += x[2]+y[2]-1.01*x[1] + 1.003*z[1]-z[2]== -200, "contrainte mars"
prob += x[3]-1.02*y[0]-1.01*x[2] + 1.003*z[2]-z[3]==200, "contrainte avril"
prob += x[4]-1.02*y[1]-1.01*x[3] + 1.003*z[3]-z[4]==-50, "contrainte mai"
prob += x[5]-1.02*y[2]-1.01*x[4] + 1.003*z[4]-z[5]==-300, "contrainte juin"
prob.solve()
for v in prob.variables():
    print(f"{v.name} = {v.varValue}")

print(f"Objective = {value(prob.objective)}")