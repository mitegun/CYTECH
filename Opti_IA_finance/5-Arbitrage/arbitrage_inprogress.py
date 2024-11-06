from itertools import permutations
import pandas as pd
import math
import networkx as nx

# Lire le fichier CSV contenant les taux de change
df = pd.read_csv('5-Arbitrage/tauxchange.csv')
devises = list(df.columns)[1:]

# Créer un graphe orienté
G = nx.DiGraph()
for i, devise1 in enumerate(devises):
    for j, devise2 in enumerate(devises):
        if i != j:
            rate = float(df.iat[j, i + 1])
            G.add_edge(devise1, devise2, weight=math.log2(rate))
cycles = list(nx.simple_cycles(G))

print(len(cycles))

# Trouver le cycle le plus rentable
best_cycle = max(
    (cycle for cycle in cycles if cycle),
    key=lambda cycle: sum(G[cycle[i]][cycle[(i + 1) % len(cycle)]]['weight'] for i in range(len(cycle))),
    default=[]
)

print(best_cycle)

# Calculer la valeur finale après arbitrage
final_value = 1
for i in range(len(best_cycle)):
    devise1 = best_cycle[i]
    devise2 = best_cycle[(i + 1) % len(best_cycle)]
    final_value *= 2 ** G[devise1][devise2]['weight']
    print(final_value)

import matplotlib.pyplot as plt

nx.draw_shell(G, with_labels=True, font_weight='bold')
plt.show()